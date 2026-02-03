module Year2025Day5_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

type ItemNumber = int64

let pullFreshRange (line:string) : (ItemNumber * ItemNumber) =
    let parsed = line.Split("-")
                            |> Array.map ItemNumber.Parse
    (parsed[0], parsed[1])

let isFresh (itemNumber:ItemNumber) (freshRanges:(ItemNumber * ItemNumber)[]) : bool =
    freshRanges
        |> Array.exists (fun (low, high) -> itemNumber >= low && itemNumber <= high) 

type State = {
    result: (ItemNumber * ItemNumber)[]
    current: int
}

let collapseRanges (ranges:(ItemNumber * ItemNumber)[]) : (ItemNumber * ItemNumber)[] =
    
    let sortedRanges = Array.sort ranges |> List.ofArray

    let collapsed = sortedRanges
                    |> List.fold (fun acc (s,e) ->
                                        match acc with
                                        | [] -> [(s,e)]
                                        | (cs,ce)::rest when s <= ce ->
                                            (cs, max ce e)::rest
                                        | _ ->
                                            (s,e)::acc
                                    ) []
    
    let result = collapsed |> List.rev |> Array.ofList
    result


let solve =
    let stopWatch = Stopwatch.StartNew()

    // let lines = Common.getSampleDataAsArray 2025 5
    let lines: string array = Common.getChallengeDataAsArray 2025 5

    // printfn "%A" lines
    
    let index = lines |> Array.findIndex String.IsNullOrEmpty
    let top = lines[..index-1]
    let bottom = lines[index+1..]
    // printfn "%A" top
    // printfn "%A" bottom

    let ranges = top |> Array.map pullFreshRange
    
    let items = bottom |> Array.map ItemNumber.Parse
    let freshItems = items 
                    |> Array.filter (fun item -> 
                                        Array.exists (fun (start, stop) -> 
                                                        item >= start && item <= stop
                                                      ) ranges
                                    )
    
    // printfn "%A" ranges
    // printfn "%A" freshItems

    printfn "Part 1: %d" freshItems.Length

    let collapsedRanges = collapseRanges ranges

    printfn "Ranges: %A" ranges
    printfn "Sorted ranges: %A" collapsedRanges

    let part2Result =
        collapsedRanges |> Array.sumBy (fun (r1, r2) -> r2 - r1 + 1L)

    printfn "Part 2 result is %A" part2Result
    ()