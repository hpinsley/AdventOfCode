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
    

let part1 (lines:string[]) : unit =
    let (list1, list2) = getLists lines
    ()

let solve =
    //let lines = Common.getSampleDataAsArray 2021 1
    let lines = Common.getSampleDataAsArray 2024 1
    part1(lines)

    //part2(lines)
    ()