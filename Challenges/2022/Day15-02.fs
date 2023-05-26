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


let solvePart2 (readings:Reading list) (row:int): int =
    let ranges = getRowZones readings row
    ranges.Length

let solve =
    let lines = Common.getSampleDataAsArray 2022 15
    let row = 11
    
    //let lines = Common.getChallengeDataAsArray 2022 15
    //let row = 2_000_000

    // let lines = [| "Sensor at x=5, y=5: closest beacon is at x=5, y=8" |]
    // let lines = [| "Sensor at x=8, y=7: closest beacon is at x=2, y=10" |]
    // let lines = [| "Sensor at x=0, y=0: closest beacon is at x=4, y=0" |]
    // printAllLines lines

    let readings = lines |> Seq.map parseLine |> List.ofSeq
    // printfn "All readings: %A" readings
    
    let count = solvePart2 readings row

    //printfn "Final state"
    //printf "%A" finalState

    //printfn "\nWe cleared %d cells\n" finalState.cleared.Count

    //for p in finalState.cleared |> Seq.sortBy GetY do
    //    printfn "Cleared: %A" p

    // displayAsGrid finalState.cleared readings
    printfn "Done with count = %d" count
    ()

