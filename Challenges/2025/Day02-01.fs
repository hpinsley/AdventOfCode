module Year2025Day2_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Number = uint64

type Range = 
    {
        low: Number
        high: Number 
    }

let isInvalidCodeForPartOne (v: Number) : bool =
    let s = v.ToString()
    // printfn "Checking %A (%A)" v s
    let codeLength = s.Length
    let isEven = codeLength % 2 = 0
    
    if isEven then
        let l = codeLength / 2
        let left = s[0..l-1]
        let right = s[l..]
        left = right
    else
        false

let isWordOnlyMadeUpOfTwoOrMoreSubstrings (word: string) (substring: string) : bool =
    
    if word.Length % substring.Length <> 0  // It has to fit evenly
    then
        false
    else
        let repCount = word.Length / substring.Length
        if repCount = 1
        then
            false
        else
            let constructed = String.replicate repCount substring
            constructed = word


let isInvalidCodeForPartTwo (v: Number) : bool =
    let s = v.ToString()
    // printfn "\nChecking word %s" s

    // printfn "Checking %A (%A)" v s
    let codeLength = s.Length
    let half = codeLength / 2

    if half = 0
    then
        false
    else
        // If half if 4, generate 4, 3, 2, 1
        let subStringLenghs = seq { half.. -1 .. 1 }
        // printfn "%A -> %A" half (List.ofSeq subStringLenghs)
        let substrings = subStringLenghs |> Seq.map (fun i -> s[0..i-1])
        // printfn "%A" (List.ofSeq substrings)
        let isInvalid = substrings |> Seq.map (isWordOnlyMadeUpOfTwoOrMoreSubstrings s)
        // printfn "%A" (List.ofSeq isInvalid)
        isInvalid |> Seq.exists id


let locateInvalidsForRange (validator: Number -> bool) (range: Range): Number[] =
    // printfn "Checking range: %A - %A" range.low range.high
    // Construct a sequence
    let s = seq { range.low..range.high }
    let qualifies = s |> Seq.filter validator
    qualifies |> Array.ofSeq

let part1 (ranges:Range array) : Number =
    let f = (locateInvalidsForRange isInvalidCodeForPartOne)

    ranges 
        |> Array.map f 
        |> Array.concat
        |> Array.sum

let part2 (ranges:Range array) : Number =
    let f = (locateInvalidsForRange isInvalidCodeForPartTwo)

    ranges 
        |> Array.map f 
        |> Array.concat
        |> Array.sum

let parseForRanges (line:string) : Range[] =
    line.Split(',')
        |> Array.map (fun s -> 
                        let parts = s.Split("-")
                        { low = UInt64.Parse parts[0]; high = UInt64.Parse parts[1] })


let solve =
    // let line = Common.getSampleData 2025 2
    let line = Common.getChallengeData 2025 2
    // printfn "Input text: %A" line

    let ranges = parseForRanges line

    for range in ranges do
        printfn "%u - %u: %u values" range.low range.high (range.high - range.low)

    let numberCount = ranges |> Array.sumBy (fun r -> r.high - r.low)
    printfn "Total numbers: %u" numberCount

    let part1Result = part1 ranges
    printfn "Part1 result: %A" part1Result

    let part2Result = part2 ranges
    printfn "Part2 result: %A" part2Result

    ()