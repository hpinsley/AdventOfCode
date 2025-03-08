module Year2021Day1_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let countIncreased (values:int[]) : int =
    let state = (0, Int32.MaxValue)
    let (increaseCount, _) =
        values |> Array.fold
                    (fun (incCount, prevValue) v ->
                        if (v > prevValue)
                        then
                            (incCount + 1, v)
                        else
                            (incCount, v)
                    )
                    state
    increaseCount

let part1 (lines:string[]) : unit =
    let values = lines |> Array.map int
    let result = countIncreased values
    printfn "Part1: %d increases" result

let part2 (lines:string[]) : unit =
    let values = lines |> Array.map int
    let windowed = values |> Array.windowed 3
    let windowSums = windowed |> Array.map Array.sum
    let result = countIncreased windowSums
    printfn "Part2: %d increases" result

let solve =
    //let lines = Common.getSampleDataAsArray 2021 1
    let lines = Common.getChallengeDataAsArray 2021 1
    part1(lines)

    part2(lines)
    ()