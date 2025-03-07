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
type T_HASMOVED = bool

type CompactEntry =
    | Empty of T_FILESIZE
    | File of (T_FILESIZE * T_FILESIZE * T_HASMOVED)

type State =
    {
        blockContents: int[]
        freeList: int list
        totalBlocks: int
    }

let buildState (entries:CompactEntry[]) : State =
    let totalBlocks = entries |> Array.sumBy (fun entry -> match entry 
                                                            with 
                                                                | Empty size -> size
                                                                | File (_, size, _) -> size
                                             )
    
    let blockContents = Array.create totalBlocks FREE_BLOCK
    let freeList = List<int>()

    let mutable nextBlock = 0
    for entry in entries do
        match entry with
            | File (fileno, size, false) ->
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

let rec compactTheDiskPart1Strategy (state: State) : State =
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
                    File (index / 2, v, false)
                else
                    Empty v) 
            compactMap

let part1 (compactEntries:CompactEntry[]) : uint64 =
    let initialState = buildState compactEntries
    let mutableCopy = Array.copy initialState.blockContents;
    let finalState = compactTheDiskPart1Strategy { initialState with blockContents = mutableCopy }

    let disk = finalState.blockContents[0..finalState.totalBlocks - 1]
    computeChecksum disk

let part2 (compactEntries:CompactEntry[]) : uint64 =
    // Strategy:
    // Iterate this array from the right stopping at every file.  For each file iterate
    // the array from the left examing every free block.
    // If the free block is big enough, determine the difference.  We would like to move the file
    // to the free block, but there often will be extra free.  That would require us to shift everything
    // to the right which will be slow.  Do we need a doubly linked list?
    printfn "There are %d entries in the array for part 2" compactEntries.Length
    0UL
let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2024 9
    // let lines = Common.getChallengeDataAsArray 2024 9

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