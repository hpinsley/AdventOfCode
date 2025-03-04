module Year2024Day8_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

let Obstacle = '#'

type Location =
    {
        row: int
        col: int
    }


type MoveDirection = 
    {
        deltaX: int
        deltaY: int
    }

let up = { deltaX = 0; deltaY = -1 }
let down = { deltaX = 0; deltaY = 1 }
let right = { deltaX = 1; deltaY = 0 }
let left = { deltaX = -1; deltaY = 0 }

let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2024 8
    // let lines = Common.getChallengeDataAsArray 2024 8

    let rows = lines.Length
    let cols = lines[0].Length
    let possibleLocations = allRowColPairs rows cols
                            |> Seq.map (fun (r,c) -> { row = r; col = c })
                            |> List.ofSeq


    let grid = Array2D.init rows cols (fun i j -> lines[i][j])
    // printGrid grid id

    let possibleLocations = allRowColPairs rows cols
                            |> Seq.map (fun (r,c) -> { row = r; col = c })
                            |> List.ofSeq

    let antennas = possibleLocations |> List.fold (fun state loc ->
                                                    let chr = grid[loc.row, loc.col]
                                                    if chr <> '.'
                                                    then
                                                        (chr, loc) :: state
                                                    else
                                                        state
                                                  ) 
                                                  []

    let part1Time = stopWatch.ElapsedMilliseconds
 
    stopWatch.Restart()

    // Part 2


    let part2Time = stopWatch.ElapsedMilliseconds;
    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()