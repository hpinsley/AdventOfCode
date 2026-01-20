module Year2025Day3_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let rec getMaxVoltageSubstring (input:string) (cellLength:int): string =    
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
            let result = maxDigit + getMaxVoltageSubstring remaining (cellLength - 1)
            result
    



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
    let test = "123456789123456789123456789"
    let joltage = getMaxVoltageSubstring test 1
    printfn "%s\n" joltage
    let x = 1
    
    // let lines = Common.getSampleDataAsArray 2025 3
    // let lines = Common.getChallengeDataAsArray 2025 3
    
    
    // printfn "Input text: %A" lines
    // let part1Result = part1 lines
    // printfn "Part1 result: %A" part1Result

    ()