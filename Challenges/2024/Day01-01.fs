module Year2024Day1_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let getLists (lines:string[]) : int[] * int[] =
    lines 
        |> Array.map (fun s -> s.Split("   "))
        |> Array.map (fun v -> Array.map (fun v2 -> parseInt(v2)) v)
        |> Array.map (fun pair -> (pair[0], pair[1]))
        |> Array.unzip
    

let part1 (lines:string[]) : int[] * int[] =
    let (list1, list2) = getLists lines
    let sorted1 = list1 |> Seq.sort |> Seq.toArray
    let sorted2 = list2 |> Seq.sort |> Seq.toArray

    let diffs = Array.map2 (fun v1 v2 -> abs (v2 - v1)) sorted1 sorted2
    let answer = diffs |> Seq.sum
    dump "Answer" answer
    (sorted1, sorted2)

let part2 (list1:int[], list2:int[]) : int =
    list1
        |> Array.fold (fun acc v -> acc + v * (list2 |> Array.filter (fun v2 -> v2 = v) |> Array.length )) 0

let solve =
    //let lines = Common.getSampleDataAsArray 2024 1
    let lines = Common.getChallengeDataAsArray 2024 1
    let sorted1, sorted2 = part1(lines)

    let part2Solution = part2(sorted1, sorted2)
    dump "Part 2 solution" part2Solution

    ()