module Year2022Day15_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Point = int * int
type Reading = {
    sensor: Point
    beacon: Point
    distance: int
}

type State = {
    visited: Map<Point,int>;
    cleared: Set<Point>;
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

let rec processNeighbor (fromPoint:Point) (maxDistance:int) (state:State) (p:Point) : State =
    printfn "Processing point %A.  Max distance of %d from fromPoint at %A" p maxDistance fromPoint

    let d = getDistance p fromPoint
    //printfn "The distance from %A to %A is %d" fromPoint p d

    //if (Set.contains p state.visited && false)
    if (Map.containsKey p state.visited && (d >= (Map.find p state.visited)))
    then
        printfn "Skipping point %A as it was already visited with distance %d" p (Map.find p state.visited) 
        state
    elif (d > maxDistance)
    then
        printfn "Point %A is TOO FAR from point %A (max dist is %d)" p fromPoint d
        state
    else
        printfn "Point %A is cleared at distance %d from fromPoint at %A" p (getDistance p fromPoint) fromPoint
        let newCleared = Set.add p state.cleared
        let newVisited = Map.add p d state.visited

        let newState = { state with cleared = newCleared; visited = newVisited }
        let neighbors = getNeighbors p
        //printfn "Recursing from %A with neighbors %A" p neighbors
        let nextState = neighbors
                            |> List.fold (processNeighbor p (maxDistance - 1)) newState
        nextState

let rec processCloseNeighbor (sensor:Point) (maxDistance:int) (state:State) (p:Point) : State =
    //printfn "\nProcessing CLOSE point %A.  Max distance of %d from sensor at %A" p maxDistance sensor    
    //let newState = { state with visited = Set.empty |> Set.add sensor }
    //processNeighbor sensor maxDistance newState p

    processNeighbor sensor maxDistance state p

let processReading  (state:State) (reading:Reading) : State =

    printfn "\n\nProcessing sensor at %A and beacon at %A" reading.sensor reading.beacon

    let visited = Map.empty |> Map.add reading.sensor reading.distance
    let cleared = state.cleared |> Set.add reading.sensor |> Set.add reading.beacon
    let updatedState = { state with visited = visited; cleared = cleared }

    let neighbors = getNeighbors reading.sensor
    printfn "Sensor %A neighbors are %A\n" reading.sensor neighbors
    let newState = neighbors
                    |> List.fold (processCloseNeighbor reading.sensor (reading.distance - 1)) updatedState

    newState

let displayAsGrid (points:seq<Point>) (reading:Reading) =
    let minCol = points |> Seq.map GetX |> Seq.min
    let maxCol = points |> Seq.map GetX |> Seq.max
    let minRow = points |> Seq.map GetY |> Seq.min
    let maxRow = points |> Seq.map GetY |> Seq.max
    let rows = (maxRow - minRow) + 1
    let cols = (maxCol - minCol) + 1

    let grid = Array2D.createBased minRow minCol rows cols Free
    points |> Seq.iter (fun (x,y) -> grid[y,x] <- Cleared)
    grid[GetY reading.sensor, GetX reading.sensor] <- Sensor
    grid[GetY reading.beacon, GetX reading.beacon] <- Beacon

    printGrid grid (fun b -> 
                        match b with 
                            | Free -> ' '
                            | Cleared -> '#'
                            | Sensor -> 'S'
                            | Beacon -> 'B')

let solvePart1 (readings:Reading list) : State =

    let state = {
        visited = Map.empty;
        cleared = Set.empty;
    }

    let finalState = readings
                        |> List.fold processReading state

    finalState

let solve =
    // let lines = Common.getSampleDataAsArray 2022 15
    // let lines = Common.getChallengeDataAsArray 2022 15

    // let lines = [| "Sensor at x=5, y=5: closest beacon is at x=5, y=8" |]
    // let lines = [| "Sensor at x=8, y=7: closest beacon is at x=2, y=10" |]
    let lines = [| "Sensor at x=0, y=0: closest beacon is at x=4, y=0" |]
    printAllLines lines

    let readings = lines |> Seq.map parseLine |> List.ofSeq
    printfn "All readings: %A" readings
    
    let finalState = solvePart1 readings
    printfn "Final state"
    printf "%A" finalState

    printfn "\nWe cleared %d cells\n" finalState.cleared.Count

    for p in finalState.cleared |> Seq.sortBy GetY do
        printfn "Cleared: %A" p

    displayAsGrid finalState.cleared readings[0]
    ()

