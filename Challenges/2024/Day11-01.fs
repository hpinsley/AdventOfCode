module Year2024Day11_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

let solve =
    let stopWatch = Stopwatch.StartNew()

    let line = Common.getSampleData 2024 11
    // let line = Common.getChallengeData 2024 11

    printfn "Line: %s" line

    let part1Time = stopWatch.ElapsedMilliseconds

    stopWatch.Restart()

    // Part 2
    let part2Time = stopWatch.ElapsedMilliseconds;

    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()