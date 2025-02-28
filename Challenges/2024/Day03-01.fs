module Year2024Day3_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let solve =
    let text = Common.getSampleData 2024 3
    // let text = Common.getChallengeData 2024 3
    let pattern = "mul\\(([0-9]+),([0-9]+)\\)"
    printfn "Pattern: %s" pattern
    let m = Regex.Matches(text, pattern)
    ()