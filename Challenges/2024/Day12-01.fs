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

type Wall =
    | Vertical of T_X * T_Y * T_Y
    | Horizontal of T_Y * T_X * T_X

type GenericWallInfo = (int * (int * int))

type Cell =
    {
        plant: char
        region: REGION option
        walls: Wall list
        cellPerimeter: int
    }

type RegionStats = {
    region: REGION
    perimeter: int
    area: int
    cellCount: int
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
                        Some (Vertical (location.col,location.row,location.row + 1))
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
                            Some (Vertical (location.col + 1,location.row,location.row + 1))
                        else
                            let neighbor = grid[location.row, location.col + 1]
                            let neighborRegion = Option.get neighbor.region
                            if neighborRegion <> ourRegion
                            then
                                Some (Vertical (location.col + 1, location.row, location.row + 1))
                            else
                                None

    let topWall = if location.row = 0
                   then
                        Some (Horizontal (location.row,location.col,location.col + 1))
                   else
                        let neighbor = grid[location.row - 1, location.col]
                        let neighborRegion = Option.get neighbor.region
                        if neighborRegion <> ourRegion
                        then
                            Some (Horizontal (location.row, location.col, location.col + 1))
                        else
                            None

    let bottomWall =    if location.row = rows - 1
                        then
                            Some (Horizontal (location.row + 1, location.col,location.col + 1))
                        else
                            let neighbor = grid[location.row + 1, location.col]
                            let neighborRegion = Option.get neighbor.region
                            if neighborRegion <> ourRegion
                            then
                                Some (Horizontal (location.row + 1, location.col, location.col + 1))
                            else
                                None

    let possibleWalls = [topWall; rightWall; bottomWall; leftWall]
    let walls = possibleWalls |> List.choose id
    
    walls
    
let totalRegion (cellTuples:(REGION*int) list) : (int * int) =
    let regionArea = cellTuples.Length
    let regionPerimeter = cellTuples |> List.sumBy (fun (_,p) -> p)
    (regionArea, regionPerimeter)

let part1 (grid:Cell[,]): int =

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

let reduceSortedHorizontalInfo (wallInfo:GenericWallInfo list) : GenericWallInfo list =

    let rec _reduceSortedHorizontalInfo (unreduced:GenericWallInfo list) (fullyReduced:GenericWallInfo list) : GenericWallInfo list = 
                                        match unreduced with
                                            | [] -> fullyReduced
                                            | seg1 :: [] -> seg1 :: fullyReduced
                                            | ((seg1rc, (seg1Start, seg1End)) as p1) :: ((seg2rc, (seg2Start, seg2End)) as p2) :: pending ->
                                                if (seg1rc = seg2rc && seg1End = seg2Start)
                                                then
                                                    let combinedWall = (seg1rc, (seg1Start, seg2End))
                                                    _reduceSortedHorizontalInfo (combinedWall :: pending) fullyReduced
                                                else
                                                    _reduceSortedHorizontalInfo (p2 :: pending) (p1::fullyReduced)
    _reduceSortedHorizontalInfo wallInfo []                                                
                                     



let getRegionSideList (walls:Wall list) : Wall list =
    // To join wall segments that are part of the same wall, we can sort first the type of wall (horizontal
    // or vertical).
    // Horizontal: Wall segments must be in the same row and the end of one matches the start of the other
    // Vertical: Wall segments must be in the same column and the end of one matches the start of the other.

    let horizontalWalls = walls |> List.choose (fun w ->
                                                    match w with
                                                        | Horizontal (y,x1,x2) -> Some (y,(x1,x2))
                                                        | _ -> None
                                                )
                                |> List.sort

    let verticalWalls = walls |> List.choose (fun w ->
                                                    match w with
                                                        | Vertical (x,y1,y2) -> Some (x,(y1,y2))
                                                        | _ -> None
                                                )
                                |> List.sort

    let reducedHorizontal = reduceSortedHorizontalInfo horizontalWalls
    let reducedVertical = reduceSortedHorizontalInfo verticalWalls
    
    // Now turn them back into Walls
    let horizontalWalls = reducedHorizontal |> List.map (fun (y, (x1, x2)) -> Horizontal (y,x1,x2))
    let verticalWalls = reducedVertical |> List.map (fun (x, (y1, y2)) -> Vertical (x, y1, y2))
    let sides = List.concat [horizontalWalls; verticalWalls]
    sides


let computePart2PriceForRegion (cells:Cell list) : int =
    let area = cells.Length
    
    // Now we need the number of sides in this region.  Each cell has a list of sides so we need to join
    // wall segments that are part of the same wall.  First pull out all the wall segments

    let wallSegments = cells |> List.map (fun c -> c.walls) |> List.concat
    let joinedWallSegments = getRegionSideList wallSegments
    let sides = joinedWallSegments.Length
    let total = area * sides
    total

let part2 (grid:Cell[,]): int =

    // Let's get lists of cells by region.

    let regions:Cell list list  = grid  |> iterate2DArray
                                        |> Seq.map (fun (i,j,c) -> 
                                                        let r = match c.region with 
                                                                    | Some r -> r
                                                                    | None -> raise (Exception("No region?"))
                                                        (r, c)
                                                    )
                                        |> Seq.groupBy fst
                                        |> Seq.map snd
                                        |> Seq.map (fun tplList -> Seq.map snd tplList |> List.ofSeq)
                                        |> List.ofSeq

    let totalPrice = regions |> List.sumBy computePart2PriceForRegion
    totalPrice
    

let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2024 12
    // let lines = Common.getChallengeDataAsArray 2024 12
    
    let rows = lines.Length
    let cols = lines[0].Length

    printfn "The grid is %d x %d" rows cols

    let grid = Array2D.init rows cols (fun r c -> { plant = lines[r][c]; region = None; cellPerimeter = -1; walls = [] })
    //printGrid grid (fun cell -> cell.plant)

    // Assign every cell to a region
    buildRegions grid

    // Now find all the walls
    Array2D.iteri (fun r c cell ->
                    let location = { row = r; col = c }
                    let walls = determineCellWals grid location
                    grid[r,c] <- { cell with cellPerimeter = walls.Length; walls = walls }
                   ) grid

    let part1Result = part1 grid
    let part1Time = stopWatch.ElapsedMilliseconds

    printfn "Part 1 result is %d" part1Result
    stopWatch.Restart()

    // Part 2
    let part2Result = part2 grid
    let part2Time = stopWatch.ElapsedMilliseconds

    printfn "Part 2 result is %d" part2Result

    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()