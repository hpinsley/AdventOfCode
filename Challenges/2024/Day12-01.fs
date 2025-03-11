module Year2024Day12_Part1

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

type REGION = Location     // A region is identified by the first location encountered

type Cell =
    {
        plant: char
        region: REGION option
    }


let getNeighbors (rows:int) (cols:int) (location:Location) : Location list =
    let possibles = [ 
                      { row = location.row - 1; col = location.col  }
                      { row = location.row + 1; col = location.col }
                      { row = location.row; col = location.col - 1 }
                      { row = location.row; col = location.col + 1 }
    ]
    possibles |> List.filter (fun loc -> loc.row >= 0 && loc.row < rows && loc.col >= 0 && loc.col < cols)

let rec floodFillRegion (grid:Cell[,]) (location:Location) (region:REGION) : unit =
    match grid[location.row,location.col].region with
        | Some _ -> ()          // This cell is already assigned a region
        | None ->
            let rows = Array2D.length1 grid
            let cols = Array2D.length2 grid

            grid[location.row,location.col] <- { grid[location.row,location.col] with region = Some region}
            let neighbors = getNeighbors rows cols location
            neighbors |> List.iter (fun neighborLoc -> (need to check plant here)floodFillRegion grid neighborLoc region)
    ()

let rec floodFill (grid:Cell[,]) (location:Location) : unit =
    match grid[location.row,location.col].region with
        | Some _ ->
            ()      // Just return as this cell has already been assigned a region
        
        | None ->   // This is the first
            let newRegion = location   // Identify the region by its first cell
            floodFillRegion grid location newRegion
            
let buildRegions (grid:Cell[,]): unit =
    Array2D.iteri (fun r c _ -> floodFill grid { row = r; col = c }) grid

let part1 (grid:Cell[,]): int =
    buildRegions grid
    0
    
let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2024 12
    // let lines = Common.getChallengeDataAsArray 2024 12
    
    let rows = lines.Length
    let cols = lines[0].Length

    printfn "The grid is %d x %d" rows cols

    let grid = Array2D.init rows cols (fun r c -> { plant = lines[r][c]; region = None })
    printGrid grid (fun cell -> cell.plant)

    let part1Result = part1 grid

    let part1Time = stopWatch.ElapsedMilliseconds
    stopWatch.Restart()

    // Part 2
    let part2Time = stopWatch.ElapsedMilliseconds;

    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()