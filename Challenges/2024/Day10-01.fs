module Year2024Day10_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2024 10
    // let lines = Common.getChallengeDataAsArray 2024 10

    let rows = lines.Length
    let cols = lines[0].Length
    printf "Rows: %d, Cols: %d\n" rows cols

    let grid = Array2D.init rows cols (fun i j -> lines[i][j])
    printGrid grid id
    let part1Time = stopWatch.ElapsedMilliseconds

    stopWatch.Restart()

    // Part 2
    let part2Time = stopWatch.ElapsedMilliseconds;

    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()