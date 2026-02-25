module Year2025Day8_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2025 8
    // let lines: string array = Common.getChallengeDataAsArray 2025 8
    printfn "%A" lines

    ()