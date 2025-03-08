module Year2021Day7_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let solve =
    // let lines = Common.getSampleDataAsArray 2021 7
    let lines = Common.getChallengeDataAsArray 2021 7

    printAllLines lines
    let positions = lines[0].Split(',') |> Array.map int

    printfn "%A" positions
    let avg = (positions |> Array.sum |> double) / ((double) positions.Length)
    printfn "There are %d positions with an average horizontal position = %f" positions.Length avg

    let v1 = positions |> Array.min
    let v2 = positions |> Array.max
    printfn "Min %d; Max %d; Range: %d" v1 v2 (v2 - v1)

    let part1MoveResults = seq { v1 .. v2 }
                        |> Seq.map (fun target ->
                                        let moves = positions |> Array.map (fun p -> abs (p - target)) |> Array.sum
                                        (target, moves)
                                    )
    printfn "Part1: %A" (part1MoveResults |> Seq.minBy snd)

    let part2MoveResults = seq { v1 .. v2 }
                        |> Seq.map (fun target ->
                                        let moves = positions |> Array.map (fun p -> 
                                                                                let n = abs (p - target)
                                                                                (n * (n+1)) / 2
                                                                           ) |> Array.sum
                                        (target, moves)
                                    )
    printfn "Part2: %A" (part2MoveResults |> Seq.minBy snd)