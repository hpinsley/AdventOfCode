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
type Problem = OneProblem of Operation * Operand[]

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

    let twoDArray = Array2D.init cols rows (fun r c -> components[c][r])
    twoDArray


let solveProblem (problem:Problem): Operand =
    match problem with
        | OneProblem (operator, operands) ->
                let binaryOperator = match operator with
                                            | Add -> (+)
                                            | Multiply -> (*)
                Array.reduce binaryOperator operands

let solveAllProblems (problems:Problem[]) : Operand =
    problems |> Array.sumBy solveProblem

let parseInputData (lines:string[]): Problem[] =
    let composed = parseComponents lines
    printfn "Composed"
    printfn "%A" composed

    printfn ""

    let transposed = transposeStrings composed
    printfn "Transposed"
    printfn "%A" transposed
    let rowCount = Array2D.length1 transposed
    let colCount = Array2D.length2 transposed
    printfn "There are %d rows and %d cols" rowCount colCount

    let problems = seq {0..rowCount-1} 
                            |> Seq.map (fun r -> 
                                            let op = match transposed[r,colCount - 1] with
                                                                    | "+" -> Add
                                                                    | "*" -> Multiply
                                                                    | _ -> raise (Exception "Unknown character")
                                            // We need to put row r, cols 0..colCount -2 into an array
                                            let operands = seq { 0 .. colCount - 2}
                                                                |> Seq.map (fun c -> transposed[r, c])
                                                                |> Seq.map (fun s -> Operand.Parse(s))
                                                                |> Array.ofSeq

                                            OneProblem (op, operands)
                                        )
                            |> Array.ofSeq
    problems

let convertPart1OperandsToPart2 (operands:Operand []) : Operand[] =
    operands
    
let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2025 6
    // let lines: string array = Common.getChallengeDataAsArray 2025 6

    printfn "%A" lines
    let problems = parseInputData lines
    printfn "%A" problems

    let part1Result = solveAllProblems problems
    printfn "Part 1: %A" part1Result
    ()