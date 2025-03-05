﻿module Year2024Day9_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

let solve =
    let stopWatch = Stopwatch.StartNew()

    // let lines = Common.getSampleDataAsArray 2024 9
    let lines = Common.getChallengeDataAsArray 2024 9

    let compactMap = lines[0] |> Array.ofSeq |> Array.map (fun c -> parseInt (c.ToString()))

    let part1Time = stopWatch.ElapsedMilliseconds
 
    stopWatch.Restart()

    // Part 2

    let part2Time = stopWatch.ElapsedMilliseconds;
    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()