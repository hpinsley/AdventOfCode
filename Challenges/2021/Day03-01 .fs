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

let getColCount (lines:string[]) (c:int) : ColCounts =
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
        leastCommonBit = if (ones < zeros) then 1 else 0
        mostCommonBit = if (zeros > ones) then 0 else 1
    }

let getColumnBitCounts (lines:string[]) : ColCounts[] =
    let columns = lines[0].Length
    let colCounts = seq { 0 .. columns - 1 }
                        |> Seq.map (getColCount lines)
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

let filterValuesUntilOneRemaining (lines:string[]) (bitToUse:ColCounts -> int) : int =
    let columns = lines[0].Length

    let remaining = seq { 0 .. columns - 1}
                    |> Seq.fold (fun (codes:string[]) c ->
                                    printfn "Checking column %d" c
                                    let colCount = getColCount codes c
                                    let testBit = bitToUse colCount
                                    if (codes.Length = 1)
                                    then
                                        codes
                                    else
                                       codes
                                            |> Array.filter (fun s -> s[colCount.colIndex] = char ((int '0') + testBit))

                              ) lines
    binaryStringToInt remaining[0]

let getOxygenRating (lines:string[])  : int =
    filterValuesUntilOneRemaining lines (fun colCount -> colCount.mostCommonBit)

let getCo2Rating (lines:string[])  : int =
    filterValuesUntilOneRemaining lines (fun colCount -> colCount.leastCommonBit)

let solve =
    // let lines = Common.getSampleDataAsArray 2021 3
    let lines = Common.getChallengeDataAsArray 2021 3
    // printAllLines lines

    let colCounts = getColumnBitCounts lines
    // printfn "%A" colCounts
    let (epsilon, gamma) = computeValues colCounts
    let powerConsumption = epsilon * gamma
    printfn "Epsilon = %d, Gamma = %d, Power consumption = %d" epsilon gamma powerConsumption
    
    let oxygenRating = getOxygenRating lines
    printfn "Oxygen rating is %d" oxygenRating
    let co2Rating = getCo2Rating lines
    printfn "The CO2 rating is %d" co2Rating
    let lifeSupport = oxygenRating * co2Rating
    printfn "Life support: %d" lifeSupport

    ()