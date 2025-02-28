module Year2024Day2_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let parseLines (lines:string[]) : int[][] =
    let result = lines 
                    |> Array.map (fun s -> s.Split(" "))
                    |> Array.map (fun v -> v |> Array.map (fun v2 -> parseInt(v2)))
    result

let isValidDeltaList (deltas:int[]) : bool =
    let maxDiff = 3
    if Array.exists (fun v -> v = 0) deltas
        then false
    else
        let firstElement:int = deltas[0]
        if Array.exists (fun (v:int) -> (Math.Sign v) <> (Math.Sign firstElement)) deltas
        then
            false
        else
            not (Array.exists (fun v -> (abs v) > maxDiff) deltas)

let getDiffs (report:int[]) : int[] =
    let list1 = report[0..(report.Length - 2)]   // Shift left and add a dummy element at the end
    let list2 = report[1..]
    let tupleList = Array.zip list1 list2
    tupleList |> Array.map (fun tpl -> snd tpl - fst tpl)

let part1 (data: int[][]) : int =
    let deltas = data |> Array.map getDiffs
    let countSafe = deltas |> Array.filter isValidDeltaList |> Array.length
    countSafe


let solve =
    // let lines = Common.getSampleDataAsArray 2024 2
    let lines = Common.getChallengeDataAsArray 2024 2
    let data = parseLines lines
    let part1Result = part1 data
    printfn "There are %d valid reports" part1Result
    ()