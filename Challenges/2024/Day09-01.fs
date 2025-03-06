module Year2024Day9_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

let FREE_BLOCK = -1

type CompactEntry =
    | Empty of int
    | File of (int * int)

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

let rec compactTheDisk (state: State) : State =
    if state.blockContents[state.totalBlocks - 1] = FREE_BLOCK  // Skip free blocks at the end
    then
        compactTheDisk { state with totalBlocks = state.totalBlocks - 1 }
    else
        match state.freeList with
            | [] -> state
            | freeBlock :: remainingFree ->
                // We have a free block to move.  However, it may be past where we are now
                if freeBlock < state.totalBlocks - 1
                then
                    state.blockContents[freeBlock] <- state.blockContents[state.totalBlocks - 1]
                    compactTheDisk { state with freeList = remainingFree; totalBlocks = state.totalBlocks - 1}
                else    // We are done because we shifted left and the original free block is not needed
                    state

let part1 (line:string) : uint64 =
    
    let isFile compactIndex = compactIndex % 2 = 0

    let compactMap = line |> Array.ofSeq |> Array.map (fun c -> parseInt (c.ToString()))
    let parsedMap = Array.mapi (fun index v ->
                                    if isFile index
                                    then
                                        File (index / 2, v)
                                    else
                                        Empty v) 
                                compactMap
    let initialState = buildState parsedMap
    let mutableCopy = Array.copy initialState.blockContents;
    let finalState = compactTheDisk { initialState with blockContents = mutableCopy }

    let disk = finalState.blockContents[0..finalState.totalBlocks - 1]

    let checksum = disk |> Seq.mapi (fun index fileno ->
                                        if fileno = FREE_BLOCK then 0UL
                                        else uint64(index) * uint64(fileno)
                                    )
                        |> Seq.sum

    checksum

let solve =
    let stopWatch = Stopwatch.StartNew()

    // let lines = Common.getSampleDataAsArray 2024 9
    let lines = Common.getChallengeDataAsArray 2024 9

    let part1Result = part1 lines[0]

    let part1Time = stopWatch.ElapsedMilliseconds
    printfn "Part 1 checksum is %d" part1Result
    stopWatch.Restart()

    // Part 2

    let part2Time = stopWatch.ElapsedMilliseconds;
    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()