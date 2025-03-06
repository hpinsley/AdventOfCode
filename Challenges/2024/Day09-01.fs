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
        freeList: List<int>
        totalBlocks: int
    }

let buildState (entries:CompactEntry[]) : State =
    let totalBlocks = entries |> Array.sumBy (fun entry -> match entry 
                                                            with 
                                                                | Empty size -> size
                                                                | File (fileno, size) -> size
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
        freeList = freeList
        totalBlocks = totalBlocks
    }


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
    0UL

let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2024 9
    // let lines = Common.getChallengeDataAsArray 2024 9

    let part1Result = part1 lines[0]

    let part1Time = stopWatch.ElapsedMilliseconds
 
    stopWatch.Restart()

    // Part 2

    let part2Time = stopWatch.ElapsedMilliseconds;
    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()