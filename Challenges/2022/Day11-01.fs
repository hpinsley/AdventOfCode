module Year2022Day11_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions

let solve =
    let lines = Common.getSampleDataAsArray 2022 11
    // let lines = Common.getChallengeDataAsArray 2022 10
    printfn "%A" lines
