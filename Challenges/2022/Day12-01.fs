module Year2022Day12_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type HeightMap = 
    {
        Heights: int[,]
        StartLocation: int * int
        EndLocation: int * int
    }

let getRow = fst
let getCol = snd

let HugeScore = System.Int32.MaxValue

type Neighbor =
    {
        Loc: int * int

    }
type Node =
    {
        Loc: int * int
        Parent: Option<int * int>
    }

let getNeighborLocs rowCount colCount row col =
    seq {
        if (row > 0) then 
            yield (row - 1, col)
        if (row < rowCount - 1) then
            yield (row + 1, col)
        if (col > 0) then 
            yield (row, col - 1)
        if (col < colCount - 1) then
            yield (row, col + 1)
    }

let getWeightedNeighbors (row:int) (col:int) (grid:int[,]): seq<((int * int) * int)> =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid

    let currentCellHeight = grid[row,col]

    let neighborHeights =
        getNeighborLocs rows cols row col
            |> Seq.filter (fun (r,c) -> (grid[r,c] - currentCellHeight) <= 1)
            |> Seq.map (fun (r,c) -> ((r,c), grid[r,c] - currentCellHeight))

    // We don't allow stepping up more than one level at a time    
    neighborHeights |> Seq.map (fun ((r,c), h) -> if h > 1 then ((r,c), HugeScore) else ((r,c), h))

let getHeightMap (lines:string[]) : HeightMap =
    let rows = lines.Length
    let cols = lines[0].Length

    let grid = Array2D.create rows cols 0
    let mutable startLocation = (0, 0)
    let mutable endLocation = (0, 0)

    for r = 0 to rows - 1 do
        for c = 0 to cols - 1 do
            let h = lines[r][c]
            match h with
                | 'S' -> grid[r,c] <- int 'a' - int 'a'
                         startLocation <- (r,c)
                | 'E' -> grid[r,c] <- int 'z' - int 'a'
                         endLocation <- (r,c)
                | _ -> grid[r,c] <- int h - int 'a'

    let heights = {
        Heights = grid
        StartLocation = startLocation
        EndLocation = endLocation
    }
    
    heights

// Manhattan distance
let h (cell:(int * int)) (goal:(int * int)) (heights:int[,]): int =
    let rowDiff = getRow cell - getRow goal |> abs
    let colDiff = getCol cell - getCol goal |> abs
    rowDiff + colDiff

let hCrow (cell:(int * int)) (goal:(int * int)) (heights:int[,]) : int =
    let (r,c) = cell
    let (gr, gc) = goal
    let myHeight = heights[r,c]
    let goalHeight = heights[gr, gc]
    abs (goalHeight - myHeight)

let rec reconstructPath (cameFrom:Map<(int * int), (int * int)>) (current:int * int) (subPath:(int * int) list) : (int * int) list =
    let path = current :: subPath
    match Map.tryFind current cameFrom with
        | Some pred -> reconstructPath cameFrom pred path
        | None -> path

let showRoute (paths:(int * int) list) (rows:int) (cols:int) =
    let coordGrid = Array2D.init rows cols (fun i j -> (i, j))
    let successors = List.append paths[1..] [(HugeScore, HugeScore)]
    let zipped = List.zip paths successors
    let map = Map.ofList zipped
    let f = fun (r,c) -> match Map.tryFind (r,c) map with
                            | None -> '.'
                            | Some (nr, nc) ->
                                if ((nr, nc) = (HugeScore, HugeScore))
                                then '#'
                                else
                                    if nr = r
                                        then if nc > c then '>' else '<'
                                    else
                                        if nr > r then 'v' else '^'
    printGrid coordGrid f

let findPath (startLocation:int * int) (heights:int[,]) (endLocation:int * int) : (int * int) list =
    let rows = Array2D.length1 heights
    let cols = Array2D.length2 heights

    let mutable gScore:Map<(int * int), int> = Map.empty |> Map.add startLocation 0
    let mutable fScore:Map<(int * int), int> = Map.empty |> Map.add startLocation (h startLocation endLocation heights)
    let mutable openSet:Set<(int * int)> = Set.empty |> Set.add startLocation
    let mutable cameFrom: Map<(int * int), (int * int)> = Map.empty

    let mutable foundGoal = false

    while (not foundGoal && openSet.Count > 0) do
        let current = openSet |> Seq.minBy (fun (r,c) -> fScore[(r,c)])
        openSet <- Set.remove current openSet

        // if (current = heightMap.EndLocation)
        // The trick is not to necessarilty reach the goal; we can stop if we attain the same height as the goal
        if (current = endLocation)
        then
            printfn "Reached goal"
            foundGoal <- true
        else
            let row = getRow current
            let col = getCol current

            let gScoreCurrent = Map.find (row,col) gScore

            for ((r,c),d) in getWeightedNeighbors row col heights do
                // let tentativeGScore = gScoreCurrent + d
                let tentativeGScore = gScoreCurrent + 1
                let gScoreNeighbor = Map.tryFind (r,c) gScore |> Option.defaultValue HugeScore
                if (tentativeGScore < gScoreNeighbor)
                then
                    cameFrom <- Map.add (r,c) current cameFrom
                    gScore <- Map.add (r, c) tentativeGScore gScore
                    let fScoreNeighbor = tentativeGScore + h (r,c) endLocation heights
                    fScore <- Map.add (r, c) fScoreNeighbor fScore
                    openSet <- Set.add (r, c) openSet

    if (not foundGoal)
    then
        printfn "Fail"
        []
    else
        let path = reconstructPath cameFrom endLocation []
        path

let findMinHeightLocations (grid:int[,]) : (int * int) list =
    let mutable locs = []
    let mutable minValue = System.Int32.MaxValue

    grid |> Array2D.iter (fun height -> if height < minValue then minValue <- height)
    grid |> Array2D.iteri (fun r c height -> if height = minValue then locs <- (r,c) :: locs)
    locs
                                             
let solve =
    // let lines = Common.getSampleDataAsArray 2022 12
    let lines = Common.getChallengeDataAsArray 2022 12
    for line in lines do
        printfn "%s" line

    printfn ""

    let heightMap = getHeightMap lines

    printfn ""

    printfn "%A" heightMap

    let rows = Array2D.length1 heightMap.Heights
    let cols = Array2D.length2 heightMap.Heights
    printfn "There are %d rows and %d cols" rows cols

    let endLocation = heightMap.EndLocation
    
    let startLocations = findMinHeightLocations heightMap.Heights
    printfn "%A" startLocations

    let paths = startLocations
                    |> List.fold (fun paths startLocation ->
                                        let path = findPath startLocation heightMap.Heights endLocation
                                        (path, path.Length - 1, startLocation) :: paths) []

                    |> List.filter (fun (path, steps, startLocation) -> not (List.isEmpty path)) 

    let (bestPath, steps, startLocation) = paths |> List.minBy (fun (_, steps, _) -> steps)
    showRoute bestPath rows cols
    printfn "Found %d viable paths" paths.Length
    printfn "Steps from %A to %A = %d" startLocation endLocation steps

    //for startLocation in startLocations do
    //    let path = findPath startLocation heightMap.Heights endLocation
    //    //showRoute path rows cols
    //    printfn "Steps from %A to %A = %d" startLocation endLocation (path.Length - 1)
