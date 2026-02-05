module Year2025Day6_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

type Operation =
    | Add
    | Multiply

type Operand = int64
type Problem = Operation of Operand[]

let parseComponents (lines:string[]) : string[][] =
    let x = lines 
                |> Array.map (fun line -> 
                                let trimmedLine = line.Trim()
                                let parts = trimmedLine.Split(" ") 
                                let components = Array.filter (fun s -> not (String.IsNullOrWhiteSpace(s))) parts

                                components
                             )
    x

let transposeStrings (components:string[][]) : string[,] =
    let rows = components.Length
    let cols = components[0].Length
    printfn "Rows: %d, Cols: %d" rows cols

    let x = Array2D.init cols rows (fun r c -> components[c][r])
    x
    

// let parseInputData (lines:string[]): Problem[] =
//     let x = lines
//                     |> Array.map 
//     [||]


let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2025 6
    // let lines: string array = Common.getChallengeDataAsArray 2025 6

    printfn "%A" lines

    let composed = parseComponents lines
    printfn "Composed"
    printfn "%A" composed

    printfn ""

    let transposed = transposeStrings composed
    printfn "Transposed"
    printfn "%A" transposed

    ()