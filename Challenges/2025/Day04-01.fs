module Year2025Day4_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics


let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2025 4
    // let lines: string array = Common.getChallengeDataAsArray 2025 4

    let rows = lines.Length
    let cols = lines[0].Length

    let grid = Array2D.init rows cols (fun i j -> lines[i][j])
    printGrid grid id

    printfn "Timings.  %dms" stopWatch.ElapsedMilliseconds

    ()