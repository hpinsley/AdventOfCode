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
let locateInvalidsForRange (validator: Number -> bool) (range: Range): Number[] =
    printfn "Checking range: %A - %A" range.low range.high
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

let part2 () : int =
    2

let parseForRanges (line:string) : Range[] =
    line.Split(',')
        |> Array.map (fun s -> 
                        let parts = s.Split("-")
                        { low = UInt64.Parse parts[0]; high = UInt64.Parse parts[1] })


let solve =
    let line = Common.getSampleData 2025 2
    // let line = Common.getChallengeData 2025 2
    // printfn "Input text: %A" line

    let ranges = parseForRanges line

    for range in ranges do
        printfn "%u - %u: %u values" range.low range.high (range.high - range.low)

    let numberCount = ranges |> Array.sumBy (fun r -> r.high - r.low)
    printfn "Total numbers: %u" numberCount

    let part1Result = part1 ranges
    printfn "Part1 result: %A" part1Result

    // let part2Result = part2()
    // printfn "Part2 result: %A" part2Result

    ()