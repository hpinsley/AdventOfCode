module Year2021Day6_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let parseInput (lines:string[]) : int64[] =
    lines[0].Split(',') 
        |> Array.map int
        |> Array.fold (fun acc f -> 
                        acc[f] <- acc[f] + 1L
                        acc
                      )
                        (Array.create 9 0L)

let elapseDay (fish:int64[]) : int64[] =
    let newFish = Array.create 9 0L
    let spawningFish = fish[0]
    
    // Reduce day
    for i in seq { 1 .. 8 } do
        newFish[i - 1] <- fish[i]
    
    newFish[6] <- newFish[6] + spawningFish
    newFish[8] <- spawningFish

    newFish

let solve =
    // let lines = Common.getSampleDataAsArray 2021 6
    let lines = Common.getChallengeDataAsArray 2021 6
    printAllLines lines

    let fish =  parseInput lines
    printfn "%A" fish

    let finalFish = seq { 1 .. 256 } |> Seq.fold (fun fish _ -> elapseDay fish) fish
    printfn "%A" finalFish

    let result = finalFish |> Array.sum
    printfn "Final result is %d" result

    ()