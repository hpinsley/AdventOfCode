module Year2024Day7_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

type Number = uint64

type ProblemSet =
    {
        target: Number
        arguments: Number list
    }

type Operation =
    | Add
    | Multiply
    | Concat

let part1Operations = [| Add; Multiply |]

let isSovable (problem:ProblemSet) (useConcatOperator:bool): bool =
    
    let target = problem.target
    let currentResult = problem.arguments[0]
    let remaining = List.tail problem.arguments

    let rec trySolve (target:Number) (currentResult:Number) (remaining:Number list) (useConcatOperator:bool): bool =
        match remaining with
            | [] -> currentResult = target
            | head :: tail -> 
                let addResult = currentResult + head
                if trySolve target addResult tail useConcatOperator
                then
                    true
                else
                    let mulResult = currentResult * head
                    if trySolve target mulResult tail useConcatOperator
                    then
                        true
                    elif useConcatOperator
                    then
                        let concatResult = UInt64.Parse (currentResult.ToString() + head.ToString())
                        trySolve target concatResult tail useConcatOperator
                    else
                        false

    trySolve target currentResult remaining useConcatOperator


let part1 (problems:ProblemSet[]) : Number =
    let solvable = problems |> Array.filter (fun p -> isSovable p false)
    solvable |> Array.sumBy (fun p -> p.target)

let part2 (problems:ProblemSet[]) : Number =
    let solvable = problems |> Array.filter (fun p -> isSovable p true)
    solvable |> Array.sumBy (fun p -> p.target)

let solve =
    let stopWatch = Stopwatch.StartNew()
    // let lines = Common.getSampleDataAsArray 2024 7
    let lines = Common.getChallengeDataAsArray 2024 7
    
    let problems = lines 
                    |> Array.map (fun line -> line.Replace(":", ""))
                    |> Array.map (fun line -> line.Split(' '))
                    |> Array.map (fun strings -> Array.map UInt64.Parse strings)
                    |> Array.map (fun vNums -> { target = vNums[0]; arguments = Array.toList vNums[1..]})
    
    let part1Result = part1 problems

    printfn "Part 1: Result is %A" part1Result

    let part1Time = stopWatch.ElapsedMilliseconds


    stopWatch.Restart()

    // Part 2
    let part2Result = part2 problems

    printfn "Part 2: Result is %A" part2Result

    let part2Time = stopWatch.ElapsedMilliseconds;
    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()