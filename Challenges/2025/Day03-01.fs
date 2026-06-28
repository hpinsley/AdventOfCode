module Year2025Day3_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let rec getMaxVoltageSubstring (cellLength:int) (input:string) : string =    
    // Algorithm
    // Given a cell length of n, we have to pick a digit from those between 0 and length - n
    // Get the max digit in that range.  But once we determine it, we need to choose the leftmost one.
    // Then recurse

    match cellLength with
        | 0 -> ""
        | _ -> 
            let minCharactersToLeave = (cellLength - 1)
            let maxToTake = input.Length - minCharactersToLeave
            let maxDigit = input |> Seq.take maxToTake |> Seq.max |> Char.ToString

            let maxDigitIndex = input.IndexOf(maxDigit)
            let remaining = input[maxDigitIndex + 1..]
            let result = maxDigit + getMaxVoltageSubstring  (cellLength - 1) remaining
            result
    



let getMaxVoltage (digits:string) : int64 =
    let maxJoltage = seq {
                        for i in 0 .. digits.Length - 1 do
                            for j in (i+1) .. digits.Length - 1 -> digits[i].ToString() + digits[j].ToString()
                        }   |> Seq.sortDescending
                            |> Seq.head
                            |> Int64.Parse

    printfn "Max joltage: %d" maxJoltage
    maxJoltage

let part1 (lines:string[]) : int64 =
    let joltages = lines |> Array.map getMaxVoltage
    let maxJoltage = Array.sum joltages
    maxJoltage

let part2 (lines:string[]) : int64 =
    let joltages = lines |> Array.map (getMaxVoltageSubstring 12)
                                        |> Array.map (Int64.Parse)                                        
    let maxJoltage = Array.sum joltages
    maxJoltage

let solve =
    
    // let lines = Common.getSampleDataAsArray 2025 3
    let lines = Common.getChallengeDataAsArray 2025 3
    
    
    printfn "Input text: %A" lines
    let part1Result = part1 lines
    printfn "Part1 result: %A" part1Result

    let part2Result = part2 lines
    printfn "Part2 result: %A" part2Result
    ()