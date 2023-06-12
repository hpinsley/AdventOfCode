module Year2022Day18_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Point = int * int * int

let getOriginPoints (lines:string[]) : Point[] =
    lines |>
        Array.map (fun s -> 
                    let split = s.Split(',')
                    (int split[0], int split[1], int split[2])
                )

let solve =
    let lines = Common.getSampleDataAsArray 2022 18
    // let lines = Common.getChallengeDataAsArray 2022 18

    printAllLines lines
    let points = getOriginPoints lines
    printfn "%A" points
    ()
