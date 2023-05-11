module Year2022Day12_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked

let solve =
    let lines = Common.getSampleDataAsArray 2022 12
    // let lines = Common.getChallengeDataAsArray 2022 12
    for line in lines do
        printfn "%s" line

