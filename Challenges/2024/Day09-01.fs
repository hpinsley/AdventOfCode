module Year2024Day9_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

let FREE_BLOCK = -1

type T_FILENO = int
type T_FILESIZE = int
type T_OFFSET = int

type CompactEntry =
    | Empty of T_FILESIZE
    | File of (T_FILENO * T_FILESIZE)

type Part1State =
    {
        blockContents: int[]
        freeList: int list
        totalBlocks: int
    }

type FreeMapEntry =
    {
        freeSize: int
        startBlock: int
        endBlock: int
    }

type FreeMap = FreeMapEntry[]

type FileInfo =
    {
        fileno: T_FILENO
        filesize: T_FILESIZE
        startOffset: T_OFFSET
    }

type Part2State =
    {
        blockContents: int[]
        filesToMove: FileInfo list
    }

let buildPart1State (entries:CompactEntry[]) : Part1State =
    let totalBlocks = entries |> Array.sumBy (fun entry -> match entry 
                                                            with 
                                                                | Empty size -> size
                                                                | File (_, size) -> size
                                             )
    
    let blockContents = Array.create totalBlocks FREE_BLOCK
    let freeList = List<int>()

    let mutable nextBlock = 0
    for entry in entries do
        match entry with
            | File (fileno, size) ->
                Array.fill blockContents nextBlock size fileno
                nextBlock <- nextBlock + size
            | Empty size ->
                for b in { nextBlock .. nextBlock + size - 1} do
                    freeList.Add(b)
                nextBlock <- nextBlock + size

    printfn "The disk contains %d blocks" totalBlocks
    {
        blockContents = blockContents;
        freeList = List.ofSeq freeList
        totalBlocks = totalBlocks
    }

let buildPart2State (entries:CompactEntry[]) : Part2State =
    let totalBlocks = entries |> Array.sumBy (fun entry -> match entry 
                                                            with 
                                                                | Empty size -> size
                                                                | File (_, size) -> size
                                             )
    
    let blockContents = Array.create totalBlocks FREE_BLOCK

    let mutable nextBlock = 0
    let fileList = List<FileInfo>()

    for entry in entries do
        match entry with
            | File (fileno, size) ->
                Array.fill blockContents nextBlock size fileno
                fileList.Add({ fileno = fileno; filesize = size; startOffset = nextBlock })
                nextBlock <- nextBlock + size
            | Empty size ->
                nextBlock <- nextBlock + size

    fileList.Reverse();

    printfn "The disk contains %d blocks" totalBlocks

    {
        blockContents = blockContents;
        filesToMove = List.ofSeq fileList
    }

let rec compactTheDiskPart1Strategy (state: Part1State) : Part1State =
    if state.blockContents[state.totalBlocks - 1] = FREE_BLOCK  // Skip free blocks at the end
    then
        compactTheDiskPart1Strategy { state with totalBlocks = state.totalBlocks - 1 }
    else
        match state.freeList with
            | [] -> state
            | freeBlock :: remainingFree ->
                // We have a free block to move.  However, it may be past where we are now
                if freeBlock < state.totalBlocks - 1
                then
                    state.blockContents[freeBlock] <- state.blockContents[state.totalBlocks - 1]
                    compactTheDiskPart1Strategy { state with freeList = remainingFree; totalBlocks = state.totalBlocks - 1}
                else    // We are done because we shifted left and the original free block is not needed
                    state

let computeChecksum (disk:int[]) : uint64 =
    disk    |> Seq.mapi (fun index fileno ->
                        if fileno = FREE_BLOCK then 0UL
                        else uint64(index) * uint64(fileno)
                    )
            |> Seq.sum

let buildCompactMap (line:string) : CompactEntry[] =
    let isFile compactIndex = compactIndex % 2 = 0

    let compactMap = line |> Array.ofSeq |> Array.map (fun c -> parseInt (c.ToString()))
    Array.mapi (fun index v ->
                if isFile index
                then
                    File (index / 2, v)
                else
                    Empty v) 
            compactMap

let part1 (compactEntries:CompactEntry[]) : uint64 =
    let initialState = buildPart1State compactEntries
    let mutableCopy = Array.copy initialState.blockContents;
    let finalState = compactTheDiskPart1Strategy { initialState with blockContents = mutableCopy }

    let disk = finalState.blockContents[0..finalState.totalBlocks - 1]
    computeChecksum disk

let iterateFreeSpace (buffer:T_FILENO[]) : FreeMapEntry seq =
    let mutable startFreeBlock = None

    seq { 
            for index in 0.. buffer.Length - 1 do
                if buffer[index] = FREE_BLOCK
                then
                    match startFreeBlock with
                        | None -> startFreeBlock <- Some index
                        | _ -> ()
                else // We are at a file
                    match startFreeBlock with
                        | Some freeStart ->
                            let freeblock = {
                                freeSize = index - freeStart;
                                startBlock = freeStart;
                                endBlock = index - 1;
                            }
                            yield freeblock
                            startFreeBlock <- None

                        | None ->
                            ()
    }


let part2 (compactEntries:CompactEntry[]) : uint64 =
    printfn "There are %d entries in the array for part 2" compactEntries.Length

    let initialState = buildPart2State compactEntries
    let freeSpace= iterateFreeSpace initialState.blockContents |> Array.ofSeq

    let findSpace (sizeNeeded: T_FILESIZE) (maxOffset: T_OFFSET) : T_OFFSET option =
        if sizeNeeded = 0
        then
            raise (Exception("Can't search for zero blocks"))
        else
            freeSpace |> Array.tryFindIndex (fun freeMapEntry -> 
                                                    freeMapEntry.freeSize >= sizeNeeded 
                                                    && freeMapEntry.startBlock + freeMapEntry.freeSize <= maxOffset)
        
    let rec moveState (state:Part2State) : Part2State =
        match state.filesToMove with
            | [] -> state
            | fileToMove :: remainingFiles ->
                match findSpace fileToMove.filesize fileToMove.startOffset with
                    | None -> moveState { state with filesToMove = remainingFiles}
                    | Some slotIndex -> 
                        let freeSlotInfo = freeSpace[slotIndex]
                        Array.fill state.blockContents freeSlotInfo.startBlock fileToMove.filesize fileToMove.fileno
                        freeSpace[slotIndex] <- { freeSlotInfo with startBlock = freeSlotInfo.startBlock + fileToMove.filesize;
                                                                    freeSize = freeSlotInfo.freeSize - fileToMove.filesize }
                        Array.fill state.blockContents fileToMove.startOffset fileToMove.filesize FREE_BLOCK
                        moveState { state with filesToMove = remainingFiles }

    let finalState = moveState initialState
    computeChecksum finalState.blockContents

let solve =
    let stopWatch = Stopwatch.StartNew()

    // let lines = Common.getSampleDataAsArray 2024 9
    let lines = Common.getChallengeDataAsArray 2024 9

    let compactEntries = buildCompactMap lines[0]    
    let part1Result = part1 compactEntries

    let part1Time = stopWatch.ElapsedMilliseconds
    printfn "Part 1 checksum is %d" part1Result
    stopWatch.Restart()

    // Part 2
    let part2Result = part2 compactEntries

    let part2Time = stopWatch.ElapsedMilliseconds;
    printfn "Part 2 checksum is %d" part2Result

    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()