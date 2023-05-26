module Year2022Day15_Part2

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Point = int * int

type ColRange = {
    startCol: int;
    endCol: int;
}

type Reading = {
    sensor: Point
    beacon: Point
    distance: int
}

type CellType =
    | Free
    | Cleared
    | Beacon
    | Sensor

let GetX = fst
let GetY = snd

let getDistance (p1:Point) (p2:Point) : int =
    let x1 = GetX p1
    let x2 = GetX p2
    let y1 = GetY p1
    let y2 = GetY p2

    Math.Abs(x2 - x1) + Math.Abs(y2 - y1)

let getNeighbors (p:Point) : Point list =
    let x = GetX p
    let y = GetY p
    [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)]

let parseLine (line:string) : Reading =
    let m = Regex("Sensor at x=([-0123456789]+), y=([-0123456789]+): closest beacon is at x=([-0123456789]+), y=([-0123456789]+)").Match(line)
    
    let values = seq { 1 .. 4 } 
                    |> Seq.map (fun index -> int m.Groups[index].Value)
                    |> Array.ofSeq

    let sensor = (values[0], values[1]);
    let beacon = (values[2], values[3])

    let (reading:Reading) = {

        sensor = sensor;
        beacon = beacon;
        distance = getDistance sensor beacon;
    }

    reading



let displayAsGrid (points:seq<Point>) (readings:Reading list) =
    let minCol = points |> Seq.map GetX |> Seq.min
    let maxCol = points |> Seq.map GetX |> Seq.max
    let minRow = points |> Seq.map GetY |> Seq.min
    let maxRow = points |> Seq.map GetY |> Seq.max
    let rows = (maxRow - minRow) + 1
    let cols = (maxCol - minCol) + 1

    let grid = Array2D.createBased minRow minCol rows cols Free
    points |> Seq.iter (fun (x,y) -> grid[y,x] <- Cleared)

    for reading in readings do
        grid[GetY reading.sensor, GetX reading.sensor] <- Sensor
        grid[GetY reading.beacon, GetX reading.beacon] <- Beacon

    printGrid grid (fun b -> 
                        match b with 
                            | Free -> ' '
                            | Cleared -> '#'
                            | Sensor -> 'S'
                            | Beacon -> 'B')
 

let getRowZoneForSingleSensor (row:int) (r:Reading) : ColRange option =
        let beaconDistance = r.distance
        let rowsFromSensor = (GetY r.sensor) - row |> Math.Abs
        let colsToClear = (beaconDistance - rowsFromSensor)

        if (colsToClear >= 0)
        then
            let center = GetX r.sensor
            let left = center - colsToClear
            let right = center + colsToClear
            
            Some {
                startCol = left;
                endCol = right;
            }
        else
            None

let getRowZones (readings:Reading list) (row:int): ColRange list =

    readings
        |> List.map (getRowZoneForSingleSensor row)
        |> List.choose id

let colRangeContains (c:int) (colRange:ColRange) : bool =
    (c >= colRange.startCol) && (c <= colRange.endCol)

let overlaps (r1:ColRange) (r2:ColRange) : bool =
    let doesOverlap = (colRangeContains r2.startCol r1)
                        ||  (colRangeContains r2.endCol r1)
                        ||  (colRangeContains r1.startCol r2)
                        ||  (colRangeContains r1.endCol r2)
    doesOverlap

let combine (r1:ColRange) (r2:ColRange) : ColRange =
    {
        startCol = Math.Min(r1.startCol,r2.startCol);
        endCol = Math.Max(r1.endCol, r2.endCol);
    }

let consolidateColRanges (colRanges:ColRange list) : ColRange list =
    let sortedRanges = colRanges |> List.sortBy (fun colRange -> colRange.startCol)

    let folder (ranges:ColRange list) (current:ColRange) : ColRange list =
        match ranges with
            | [] -> [current]
            | head :: tail ->
                if (overlaps head current)
                then
                    let combined = combine head current
                    combined :: tail
                else
                    current :: ranges
     

    let consolidatedRanges = sortedRanges |> List.fold folder []
    consolidatedRanges |> List.sortBy (fun r -> r.startCol)

let visualizeColRanges (row:int) (colRanges:ColRange list) : unit =
    let colMin = colRanges |> List.map (fun colRange -> colRange.startCol) |> List.min
    let colMax = colRanges |> List.map (fun colRange -> colRange.endCol) |> List.max
    
    // The printGrid function treats the first dimension as the rows.

    let yMin = colMin - 1 // Leave some room
    let yMax = colMax + 1
    let width = yMax - yMin + 1

    let grid = Array2D.initBased row yMin 1 width (fun x y -> Seq.exists (colRangeContains y) colRanges)
    printGrid grid (fun b -> if b then '#' else '.')
    ()


let checkRow (row:int) (minCoord:int) (maxCoord:int) (readings:Reading list) : (int * ColRange list) option =
    // printfn "Checking row %d" row

    let ranges = getRowZones readings row
    // visualizeColRanges row ranges
    
    let consolidatedRanges = consolidateColRanges ranges
    let truncatedRanges = consolidatedRanges |> List.map (fun range -> { startCol = Math.Max(range.startCol,minCoord); 
                                                                         endCol = Math.Min(range.endCol,maxCoord)
                                                                       })

    // printfn "There are %d truncated ranges for row %d: %A" truncatedRanges.Length row truncatedRanges

    if (truncatedRanges.Length = 0 || truncatedRanges.Length > 1) then
        printfn "That looks interesting"
        Some (row, truncatedRanges)
    else
        None

let solvePart2 (pivotRow:int) (minCoord:int) (maxCoord:int) (readings:Reading list) : unit =
   
    // let maxIter = 10
    let maxIter = pivotRow
    let rows = seq { for j in 0 .. maxIter do 
                        yield pivotRow + j 
                        yield pivotRow - j
                    }
    
    let possibleHit = rows |> Seq.pick (fun row -> checkRow row minCoord maxCoord readings)

    printfn "Possible hit is here %A" possibleHit

    let row = fst possibleHit
    let cleared = snd possibleHit
    let col = cleared[0].endCol + 1
    printfn "Is it row,col = %d,%d" row col

    let frequency = 4000000L * int64 col + int64 row
    printfn "Frequency %A" frequency

    ()

let solve =
    //let lines = Common.getSampleDataAsArray 2022 15
    //let coordMin = 0
    //let coordMax = 20
    
    let lines = Common.getChallengeDataAsArray 2022 15
    let coordMin = 0
    let coordMax = 4_000_000

    // let lines = [| "Sensor at x=5, y=5: closest beacon is at x=5, y=8" |]
    // let lines = [| "Sensor at x=8, y=7: closest beacon is at x=2, y=10" |]
    // let lines = [| "Sensor at x=0, y=0: closest beacon is at x=4, y=0" |]
    // printAllLines lines

    let readings = lines |> Seq.map parseLine |> List.ofSeq
    // printfn "All readings: %A" readings
    
    let pivotRow = (coordMax - coordMin) / 2
    printfn "Starting with pivot row %d" pivotRow
    solvePart2 pivotRow coordMin coordMax readings
    printfn "Done"
    ()

