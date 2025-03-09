module Year2024Day10_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

let TRAIL_START = 0
let TRAIL_END = 9

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

let getNextSteps (grid:int[,]) (location:Location) : Location list =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid

    let value = grid.[location.row, location.col]
    let neighbors = getNeighbors rows cols location
    neighbors |> List.filter (fun loc -> grid.[loc.row, loc.col] = value + 1)

let getTrailHeads (grid:int[,]) : Location list =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid

    [ for row in 0..rows-1 do
                  for col in 0..cols-1 do
                  if grid.[row, col] = TRAIL_START then yield { row = row; col = col } ]

let findPaths (grid:int[,]) (trailHead:Location) : Location list list =
    let rec _findPaths (grid:int[,]) (sofar: Location list list): Location list list =
        match sofar with
            | [] -> []
            | path :: rest ->
                match path with
                    | [] -> []
                    | head :: steps ->
                        if grid[head.row, head.col] = TRAIL_END 
                        then
                                path :: _findPaths grid rest
                        else
                            let nextSteps = getNextSteps grid head
                            let newPaths = nextSteps |> List.map (fun loc -> loc :: path)
                            _findPaths grid (newPaths @ rest)

    let allPaths = _findPaths grid [[trailHead]]
    allPaths

let pathToString (path:Location list) : string =
    path |> List.map (fun loc -> sprintf "%d,%d" loc.row loc.col) |> String.concat " -> "

let headScore (trails:Location list list) : int =
    // Get the index of the each trail end
    trails |> List.map List.head |> List.distinct |> List.length

let getScores (grid: int[,]) : int * int =
    let trailHeads = getTrailHeads grid
    let headTrails = trailHeads |> List.map (fun head -> findPaths grid head)
    let trailLengths = headTrails |> List.map List.length
    let part2Score = trailLengths |> List.sum
    let part1Score = headTrails |> List.map headScore |> List.sum
    (part1Score, part2Score)    

let solve =
    let stopWatch = Stopwatch.StartNew()

    // let lines = Common.getSampleDataAsArray 2024 10
    let lines = Common.getChallengeDataAsArray 2024 10

    let rows = lines.Length
    let cols = lines[0].Length
    printf "Rows: %d, Cols: %d\n" rows cols

    let grid = Array2D.init rows cols 
                (fun i  j ->  
                    let c = lines[i][j]
                    if c = '.' then -1
                    else
                        parseInt ((lines[i][j]).ToString())
                )
    // printGrid grid (fun i -> i.ToString()[0])   // We only expect single digit numbers

    let part1Result, part2Result = getScores grid
    printfn "Part 1: %d; Part2: %d" part1Result part2Result

    let part1Time = stopWatch.ElapsedMilliseconds

    stopWatch.Restart()

    // Part 2
    let part2Time = stopWatch.ElapsedMilliseconds;

    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()