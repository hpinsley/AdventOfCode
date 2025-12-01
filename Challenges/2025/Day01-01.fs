module Year2025Day1_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Rotation = (int * int) // (direction, steps)

let parseRotation (instruction:string) : Rotation =
    let m = Regex("(R|L)(\d+)").Match(instruction)
    if (not m.Success)
    then
        failwith "Bad match"

    let direction = if m.Groups[1].Value = "R" then 1 else -1
    let steps = int m.Groups[2].Value
    (direction, steps)

let solve =
    let lines = Common.getSampleDataAsArray 2025 1
    // let text = Common.getChallengeDataAsArray 2025 1
    printfn "Input text: %A" lines
    let rotations = lines |> Array.map parseRotation
    printfn "Parsed rotations: %A" rotations
    ()