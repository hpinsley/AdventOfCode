module Year2025Day3_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let getMaxVoltageSubstring (input:string) (cellLength:int): string =
    let left = input[0..(input.Length - cellLength)-1]
    let right = input[input.Length - cellLength..input.Length]
    ""

let getMaxVoltage (digits:string) : int =
    let maxJoltage = seq {
                        for i in 0 .. digits.Length - 1 do
                            for j in (i+1) .. digits.Length - 1 -> digits[i].ToString() + digits[j].ToString()
                        }   |> Seq.sortDescending
                            |> Seq.head
                            |> Int32.Parse

    printfn "Max joltage: %d" maxJoltage
    maxJoltage

let part1 (lines:string[]) : int =
    let joltages = lines |> Array.map getMaxVoltage
    let maxJoltage = Array.sum joltages
    maxJoltage

let solve =
    let test = "123456789"
    let two = getMaxVoltageSubstring test 2
    
    // let lines = Common.getSampleDataAsArray 2025 3
    // let lines = Common.getChallengeDataAsArray 2025 3
    
    
    // printfn "Input text: %A" lines
    // let part1Result = part1 lines
    // printfn "Part1 result: %A" part1Result

    ()