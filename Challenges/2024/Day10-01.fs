module Year2024Day10_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

type Location = 
    { 
        row: int
        col: int
    }

let getNeighbors (rows:int) (cols:int) (location:Location) : Location list =
    let possibles = [ 
                      { row = location.row - 1; col = location.col  }
                      { row = location.row + 1; col = location.col }
                      { row = location.row; col = location.col - 1 }
                      { row = location.row; col = location.col + 1 }
    ]
    possibles |> List.filter (fun loc -> loc.row >= 0 && loc.row < rows && loc.col >= 0 && loc.col < cols)

let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2024 10
    // let lines = Common.getChallengeDataAsArray 2024 10

    let rows = lines.Length
    let cols = lines[0].Length
    printf "Rows: %d, Cols: %d\n" rows cols

    let grid = Array2D.init rows cols (fun i  j ->  parseInt ((lines[i][j]).ToString()))
    printGrid grid (fun i -> i.ToString()[0])   // We only expect single digit numbers

    let part1Time = stopWatch.ElapsedMilliseconds

    stopWatch.Restart()

    // Part 2
    let part2Time = stopWatch.ElapsedMilliseconds;

    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()