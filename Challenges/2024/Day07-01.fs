module Year2024Day7_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

type ProblemSet =
    {
        target: uint64
        arguments: uint64[]
    }

let part1 (problems:ProblemSet[]) : uint64 =
    problems |> Array.sumBy (fun p -> p.target)

let solve =
    let stopWatch = Stopwatch.StartNew()
    let lines = Common.getSampleDataAsArray 2024 7
    // let lines = Common.getChallengeDataAsArray 2024 7
    
    let problems = lines 
                    |> Array.map (fun line -> line.Replace(":", ""))
                    |> Array.map (fun line -> line.Split(' '))
                    |> Array.map (fun strings -> Array.map UInt64.Parse strings)
                    |> Array.map (fun vNums -> { target = vNums[0]; arguments = vNums[1..]})
    
    let part1Result = part1 problems

    printfn "Part 1: Result is %A" part1Result

    let part1Time = stopWatch.ElapsedMilliseconds


    stopWatch.Restart()

    // Part 2

    let part2Time = stopWatch.ElapsedMilliseconds;
    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()