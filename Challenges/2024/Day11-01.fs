module Year2024Day11_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

type T_NUMBER = uint64
type T_STONE = T_NUMBER

let splitEvenStone (stone:T_STONE) : (T_STONE * T_STONE) option =
    let label = stone.ToString()
    if label.Length % 2 = 0 then
        let left = label[0..label.Length / 2 - 1] |> UInt64.Parse
        let right = label[label.Length / 2..] |> UInt64.Parse
        Some (left, right)
    else
        None
let changeStone (stone:T_STONE) : T_STONE list =
    if stone = 0UL then
        [1UL]
    else
        match splitEvenStone stone with
        | Some (left, right) -> [left; right]
        | None -> [stone * 2024UL]

let blink (stones:T_STONE list) : T_STONE list =
    stones |> List.collect changeStone

let rec part1 (stones:T_STONE list) (blinks:int): int =
    if blinks = 0
    then
        stones.Length
    else
        let expandedStones = blink stones
        part1 expandedStones (blinks - 1)


let solve =
    let stopWatch = Stopwatch.StartNew()

    // let line = Common.getSampleData 2024 11
    let line = Common.getChallengeData 2024 11
    let stones = line.Split(" ") |> Array.map uint64 |> List.ofArray

    printfn "Line: %s" line

    let blinkTimes = 25
    let part1Result = part1 stones blinkTimes
    printfn "Part 1: %d" part1Result

    let part1Time = stopWatch.ElapsedMilliseconds

    stopWatch.Restart()

    // Part 2
    let part2Time = stopWatch.ElapsedMilliseconds;

    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()