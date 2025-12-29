module Year2025Day3_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let getMaxVoltage (digits:string) : int =
    let maxJoltage = seq {
                        for i in 0 .. digits.Length - 1 do
                            for j in (i+1) .. digits.Length - 1 -> digits[i].ToString() + digits[j].ToString()
                        }   |> Seq.sortDescending
                            |> Seq.head
                            |> Int32.Parse

    printfn "Max joltage: %d" maxJoltage
    maxJoltage

let solve =
    // let lines = Common.getSampleDataAsArray 2025 3
    let lines = Common.getChallengeDataAsArray 2025 3
    printfn "Input text: %A" lines

    let joltages = lines |> Array.map getMaxVoltage
    let maxJoltage = Array.sum joltages
    printfn "Max joltage is %d" maxJoltage

    // let part1Result = part1 ranges
    // printfn "Part1 result: %A" part1Result

    // let part2Result = part2 ranges
    // printfn "Part2 result: %A" part2Result

    ()