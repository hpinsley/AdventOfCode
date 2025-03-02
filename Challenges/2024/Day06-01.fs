module Year2024Day6_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics


let solve =
    let stopWatch = Stopwatch.StartNew()
    let lines = Common.getSampleDataAsArray 2024 6
    // let lines = Common.getChallengeDataAsArray 2024 6

    let rows = lines.Length
    let cols = lines[0].Length

    let grid = Array2D.init rows cols (fun i j -> lines[i][j])
    printGrid grid id

    let part1Time = stopWatch.ElapsedMilliseconds

    stopWatch.Restart()

    let part2Time = stopWatch.ElapsedMilliseconds;
    printfn "Timings.  Part 1: %dµs, Part 2: %dµs" part1Time part2Time

    ()