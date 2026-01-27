module Year2025Day4_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics


let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2025 4
    // let lines: string array = Common.getChallengeDataAsArray 2025 4

    let rows = lines.Length
    let cols = lines[0].Length
    let grid = Array2D.init rows cols (fun i j -> lines[i][j])
    printGrid grid id

    let locsWithNeighborCount = grid 
                                    |> iterate2DArray |> Seq.filter (fun (_,_, c) -> c = '@')
                                    |> Seq.map (fun (r, c, _) -> (r, c, getNeighbors (rows, cols) (r,c)))
                                    |> Seq.map (fun (r, c, neighbors) ->
                                                (r, c, 
                                                    (neighbors 
                                                        |> Array.map (fun (r,c) -> grid[r,c]))
                                                        |> Array.filter (fun ch -> ch = '@')
                                                        |> Array.length
                                                ))
    let canReach = locsWithNeighborCount |> Seq.filter (fun (_, _, count) -> count < 4)
    let part1Result = Seq.length canReach
    printfn "Timings.  %dms" stopWatch.ElapsedMilliseconds
    printfn "Part 1: %A" part1Result
    ()