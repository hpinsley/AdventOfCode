module Year2024Day3_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type aggregate =
    {
        total: int;
        multiplier: int;
    }

let solve =
    // let text = Common.getSampleData 2024 3
    let text = Common.getChallengeData 2024 3
    let pattern = "mul\\(([0-9]+),([0-9]+)\\)"
    printfn "Pattern: %s" pattern
    let m = Regex.Matches(text, pattern)
    let total = m |> Seq.map (fun m -> parseInt(m.Groups[1].Value) * parseInt(m.Groups[2].Value)) |> Seq.sum
    printfn "Part 1 total is %d" total
    let pattern2 = "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)"
    printfn "Pattern2: %s" pattern2

    //let sample2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    //let m2 = Regex.Matches(sample2, pattern2)

    let m2 = Regex.Matches(text, pattern2)

    let result = m2 |> Seq.fold (fun info m -> 
                                        match m.Groups[0].Value with
                                            | "do()" -> { info with multiplier = 1 }
                                            | "don't()" -> { info with multiplier = 0 }
                                            | _ -> 
                                                let product = info.multiplier * parseInt(m.Groups[1].Value) * parseInt(m.Groups[2].Value)
                                                let newTotal = info.total + product
                                                { info with total = newTotal }
                                ) { total = 0; multiplier = 1 }
    
    printfn "Part 2 total is %d" result.total

    ()