module Year2024Day11_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

type T_NUMBER = int64
type T_STONE = T_NUMBER
type T_COUNT = T_NUMBER

let splitEvenStone (stone:T_STONE) : (T_STONE * T_STONE) option =
    let label = stone.ToString()
    if label.Length % 2 = 0 then
        let left = label[0..label.Length / 2 - 1] |> Int64.Parse
        let right = label[label.Length / 2..] |> Int64.Parse
        Some (left, right)
    else
        None
let changeStone (stone:T_STONE) : (T_STONE * T_STONE option) =
    if stone = 0L
    then
        (1L, None)
    else
        match splitEvenStone stone with
            | Some (left, right) -> (left, Some right)
            | None -> (stone * 2024L, None)

let rec expandSingleStone ((stone, blinks):T_STONE * T_COUNT) (stoneCount:T_COUNT) (dictionary:Dictionary<(T_STONE * T_COUNT), T_COUNT>): T_COUNT =
    if dictionary.ContainsKey((stone, blinks)) then
        let count = dictionary.[(stone, blinks)]
        //printfn "Found %A %A" (stone, blinks) count
        count
    elif blinks = 0 then
        stoneCount
    else
        match changeStone stone with
            | (left, Some right) -> 
                let leftCount = expandSingleStone (left, blinks - 1L)  1L dictionary  // Don't include this stone in the count
                let rightCount = expandSingleStone (right, blinks - 1L) 1L dictionary
                let count = (stoneCount - 1L) + leftCount + rightCount // Since our stone was replaced we decrement the count
                dictionary.[(stone, blinks)] <- count
                count
            | (left, None) -> 
                let leftCount = expandSingleStone (left, blinks - 1L) 1 dictionary
                let count = (stoneCount - 1L) + leftCount
                dictionary.[(stone, blinks)] <- count
                count


let rec part1 (pairs:(T_STONE * T_COUNT) list): T_COUNT =
    let stone = pairs |> List.head
    let dictionary = Dictionary<(T_STONE * T_COUNT), T_COUNT>()
    let singleStoneResult = expandSingleStone stone 1 dictionary

    let totalCount = pairs |> List.sumBy (fun stone -> expandSingleStone stone 1 dictionary)
    totalCount

let solve =
    let stopWatch = Stopwatch.StartNew()

    // let line = Common.getSampleData 2024 11
    let line = Common.getChallengeData 2024 11
    let stones = line.Split(" ") |> Array.map int64 |> List.ofArray

    printfn "Line: %s" line

    let blinkTimes = 75L
    let pairs = stones |> List.map (fun s -> s, blinkTimes)
  
    let part1Result = part1 pairs
    printfn "Part 1: %d" part1Result

    let part1Time = stopWatch.ElapsedMilliseconds

    stopWatch.Restart()

    // Part 2
    let part2Time = stopWatch.ElapsedMilliseconds;

    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()