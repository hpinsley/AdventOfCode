module Year2022Day22_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let parseIntoModel (lines:string[]) : unit =
    let l = lines.Length
    let top = lines[0..(l - 3)]
    let bottom = lines[l - 1]
    ()

let solve =
    let lines = Common.getSampleDataAsArray 2022 22
    // let lines = Common.getChallengeDataAsArray 2022 22
    printAllLines lines
    parseIntoModel lines
    ()
