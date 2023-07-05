module Year2022Day20_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let getInputArray (lines:string[]) : int[] =
    lines
        |> Array.map parseInt

type Cell (value:int) =
    member this.v = value
    member this.pred = this
    member this.succ = this

let parseValuesIntoRing (values:int[]) : Cell =
    let length = Array.length values
    Cell(7)

let solve =
    let lines = Common.getSampleDataAsArray 2022 20
    // let lines = Common.getChallengeDataAsArray 2022 20
    // printAllLines lines
    printfn "There are %d coordinates" lines.Length
    let values = getInputArray lines
    let ring = parseValuesIntoRing values
    ring.v <- 2
    ()
