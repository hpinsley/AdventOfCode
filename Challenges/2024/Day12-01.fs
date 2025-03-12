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

type T_X = int
type T_Y = int

type Cell =
    {
        plant: char
        region: REGION option
        cellPerimeter: int
    }

type RegionStats = {
    region: REGION
    perimeter: int
    area: int
    cellCount: int
}

type Wall =
    | Vertical of T_X * T_Y * T_Y
    | Horizontal of T_Y * T_X * T_X

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

            // Set this location to the given region
            grid[location.row,location.col] <- { grid[location.row, location.col] with region = Some region }
        
            let neighbors = getNeighbors rows cols location
            neighbors |> List.iter (fun neighborLoc ->
                                        // We only recurse if the neighbor has some plant and is not assigned
                                        // to some other region
                                        if grid[neighborLoc.row, neighborLoc.col].plant = grid[location.row, location.col].plant &&
                                            Option.isNone grid[neighborLoc.row, neighborLoc.col].region
                                        then
                                            floodFillRegion grid neighborLoc region
                                        else
                                            ()
                                    )

let rec floodFill (grid:Cell[,]) (location:Location) : unit =
    match grid[location.row,location.col].region with
        | Some _ ->
            ()      // Just return as this cell has already been assigned a region
        
        | None ->   // This is the first
            let newRegion = location   // Identify the region by its first cell
            floodFillRegion grid location newRegion
            
let buildRegions (grid:Cell[,]): unit =
    Array2D.iteri (fun r c _ -> floodFill grid { row = r; col = c }) grid

let determineCellWals (grid:Cell[,]) (location:Location) : Wall list =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid


    let ourRegion = match grid[location.row, location.col].region with
                        | Some r -> r
                        | None -> raise (Exception("No region!"))


    let leftWall = if location.col = 0
                   then
                        Some (Vertical (0,location.row,location.row + 1))
                   else
                        let neighbor = grid[location.row, location.col - 1]
                        let neighborRegion = Option.get neighbor.region
                        if neighborRegion <> ourRegion
                        then
                            Some (Vertical (location.col, location.row, location.row + 1))
                        else
                            None

    let rightWall =     if location.col = cols - 1
                        then
                            Some (Vertical (location.col,location.row,location.row + 1))
                        else
                            let neighbor = grid[location.row, location.col + 1]
                            let neighborRegion = Option.get neighbor.region
                            if neighborRegion <> ourRegion
                            then
                                Some (Vertical (location.col + 1, location.row, location.row + 1))
                            else
                                None
    []

let computePerimeter (grid:Cell[,]) (location:Location) : int =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid


    let ourRegion = match grid[location.row, location.col].region with
                        | Some r -> r
                        | None -> raise (Exception("No region!"))

    let neighbors = getNeighbors rows cols location
    // The number of neighbors tells us a few things along the outer boundary of the grid
    
    let exteriorPerimeter = match neighbors.Length with
                                | 4 -> 0        //Interior nodes have 4 neighbers so no exterior wall
                                | 3 -> 1        //Walls - walls have 3 neighbors and 1 exterior wall
                                | 2 -> 2        //Corners have two neighbors and 2 exterior walls
                                | _ -> raise (Exception("Unexpected neighbor length"))

    // Add to the exterior perimeter if any of the valid neighbors are NOT of the same region
    let interiorWalls = neighbors |> List.sumBy (fun neighbor ->
                                                    match grid[neighbor.row,neighbor.col].region with
                                                        | None -> raise(Exception("No region on neighbor"))
                                                        | Some theirRegion -> if theirRegion <> ourRegion then 1 else 0   //Don't need a wall between friends!
                                                        
                                                )

    let perimeter = exteriorPerimeter + interiorWalls
    perimeter

let totalRegion (cellTuples:(REGION*int) list) : (int * int) =
    let regionArea = cellTuples.Length
    let regionPerimeter = cellTuples |> List.sumBy (fun (_,p) -> p)
    (regionArea, regionPerimeter)

let part1 (grid:Cell[,]): int =

    // Assign every cell to a region
    buildRegions grid
    // Now compute the perimeter of each cell such that the region's perimeter is the sum of the cell's perimiter

    // Now we have to find out the perimeter of a cell

    Array2D.iteri (fun r c cell ->
                    let location = { row = r; col = c }
                    let p = computePerimeter grid location
                    grid[r,c] <- { cell with cellPerimeter = p }
                   ) grid

    // Now we can lose the 2d grid and group by region.  All we need for each cell is the region and
    // the perimeter, so map to tuples that lose the option part

    let cellInfoByRegion = grid    
                            |> iterate2DArray 
                            |> Seq.map (fun (i,j,c) -> 
                                            let r = match c.region with 
                                                        | Some r -> r
                                                        | None -> raise (Exception("No region?"))
                                            (r, c.cellPerimeter)
                                        )
                            |> List.ofSeq
                            |> List.groupBy fst
                            |> List.ofSeq

    // Now map each entry to region totals
    let totals = cellInfoByRegion |> List.map (fun (r,members) -> totalRegion members)
    let price = totals |> List.map (fun (a,p) -> a * p)
    let totalPrice = List.sum price
    totalPrice
    
let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2024 12
    // let lines = Common.getChallengeDataAsArray 2024 12
    
    let rows = lines.Length
    let cols = lines[0].Length

    printfn "The grid is %d x %d" rows cols

    let grid = Array2D.init rows cols (fun r c -> { plant = lines[r][c]; region = None; cellPerimeter = -1 })
    //printGrid grid (fun cell -> cell.plant)

    let part1Result = part1 grid
    let part1Time = stopWatch.ElapsedMilliseconds

    printfn "Part 1 result is %d" part1Result
    stopWatch.Restart()

    // Part 2
    let part2Time = stopWatch.ElapsedMilliseconds;

    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()