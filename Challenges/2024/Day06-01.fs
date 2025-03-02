module Year2024Day6_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic


let solve =
    let lines = Common.getSampleDataAsArray 2024 6
    // let lines = Common.getChallengeDataAsArray 2024 6

    let rows = lines.Length
    let cols = lines[0].Length

    let grid = Array2D.init rows cols (fun i j -> lines[i][j])
    printGrid grid id

    ()