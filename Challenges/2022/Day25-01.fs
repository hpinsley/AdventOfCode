module Year2022Day25_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let solve =
    // let lines = Common.getSampleDataAsArray 2022 25
    let lines = Common.getChallengeDataAsArray 2022 25
    printAllLines lines
    ()