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

type Column = int

let parsePart2InputData (lines:string[]): Problem[] =
    let argCount = lines.Length - 1
    // The operators are in lines[argCount]
    // They also denote the start of a problem.

    let operatorLine = lines[argCount]
    let width = operatorLine.Length


    let columnInfo= seq {0..width - 1}
                        |> Seq.fold (fun acc index ->
                                        let operatorOption = 
                                            match operatorLine[index] with
                                                | '+' -> Some Add
                                                | '*' -> Some Multiply
                                                | ' ' -> None
                                                | _ -> raise (Exception "Unknown character")

                                        match operatorOption with
                                            | None -> acc
                                            | Some op -> (op, index) :: acc
                                    )
                                    List.empty
                        |> List.rev
    
    let rec buildFullList = fun (w: int) (fullColInfo: (Operation * int * int) list) (partialColInfo: (Operation * int) list) -> 
                                    match partialColInfo with
                                        | [] -> raise (Exception "Should not get here")
                                        | (op, c) :: [] -> 
                                                (op, c, w - 1) :: fullColInfo

                                        | (op1, c1) :: (op2, c2) :: rest ->
                                            buildFullList w ((op1, c1, (c2-2)) :: fullColInfo) ((op2, c2) :: rest)

        

    let fullInfo = 
        buildFullList width List.empty columnInfo
            |> List.rev


    let problems = 
        fullInfo
            |> List.map (fun (op, c1, c2) ->
                            let operands = 
                                seq { c1 .. c2}
                                    |> Seq.map (fun colWithProblem ->
                                                    let argChars = seq { 0..argCount - 1}
                                                                        |> Seq.map (fun row -> lines[row][colWithProblem])
                                                    String(Seq.toArray argChars)
                                                        |> Operand.Parse
                                                )
                                |> Array.ofSeq
                            OneProblem (op, operands)
                        )
    problems |> Array.ofList

let parsePart1InputData (lines:string[]): Problem[] =
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

let solve =
    let stopWatch = Stopwatch.StartNew()

    // let lines = Common.getSampleDataAsArray 2025 6
    let lines: string array = Common.getChallengeDataAsArray 2025 6

    printfn "%A" lines
    // let problems = parsePart1InputData lines
    // printfn "%A" problems

    // let part1Result = solveAllProblems problems
    // printfn "Part 1: %A" part1Result

    let part2Problems = parsePart2InputData lines
    let part2Result = solveAllProblems part2Problems
    printfn "Part 2: %A" part2Result


    ()