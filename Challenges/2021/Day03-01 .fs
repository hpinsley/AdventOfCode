module Year2021Day3_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
type ColCounts =
    {
        colIndex: int
        zeros: int
        ones: int
        leastCommonBit: int
        mostCommonBit: int
    }

let getColumnBitCounts (lines:string[]) : ColCounts[] =
    let getColCount (c:int) : ColCounts =
        let (zeros, ones) = lines 
                                |> Array.map (fun line -> line[c])
                                |> Array.fold (fun (zeros, ones) c ->
                                                match c with
                                                    | '0' -> (zeros + 1, ones)
                                                    | '1' -> (zeros, ones + 1)
                                                    | _ -> failwith "Illegal binary digit"
                                      ) (0, 0)
        {
            colIndex = c
            zeros = zeros
            ones = ones
            leastCommonBit = if (zeros < ones) then 0 else 1
            mostCommonBit = if (zeros > ones) then 0 else 1
        }

    let columns = lines[0].Length
    let colCounts = seq { 0 .. columns - 1 }
                        |> Seq.map getColCount
                        |> Array.ofSeq
    colCounts

let computeValues (colCounts:ColCounts[]) : (int * int) =
    let (epsilon, gamma) = colCounts
                            |> Array.fold 
                                    (fun (e, g) colInfo ->
                                        (2 * e + colInfo.leastCommonBit,
                                         2 * g + colInfo.mostCommonBit)
                                    ) 
                                    (0, 0)
    (epsilon, gamma)

let solve =
    // let lines = Common.getSampleDataAsArray 2021 3
    let lines = Common.getChallengeDataAsArray 2021 3
    // printAllLines lines

    let colCounts = getColumnBitCounts lines
    // printfn "%A" colCounts
    let (epsilon, gamma) = computeValues colCounts
    let powerConsumption = epsilon * gamma
    printfn "Epsilon = %d, Gamma = %d, Power consumption = %d" epsilon gamma powerConsumption
    ()