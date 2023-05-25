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
    visited: Set<Point>;
    cleared: Set<Point>;
}

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
    //printfn "Processing point %A.  Max distance of %d from sensor at %A" p maxDistance fromPoint

    let d = getDistance p fromPoint
    //printfn "The distance from %A to %A is %d" fromPoint p d

    //if (Set.contains p state.visited && false)
    if (Set.contains p state.visited)
    then
        printfn "Skipping point %A as it was already visited" p
        state
    elif (d > maxDistance || maxDistance = 0)
    then
        state
    else
        printfn "Point %A is cleared at distance %d from fromPoint at %A" p (getDistance p fromPoint) fromPoint
        let newCleared = Set.add p state.cleared
        let newVisited = Set.add p state.visited

        let newState = { state with cleared = newCleared; visited = newVisited }
        let neighbors = getNeighbors p
        //printfn "Recursing from %A with neighbors %A" p neighbors
        let nextState = neighbors
                            |> List.fold (processNeighbor p (maxDistance - 1)) newState
        nextState

let rec processCloseNeighbor (sensor:Point) (maxDistance:int) (state:State) (p:Point) : State =
    //printfn "\nProcessing CLOSE point %A.  Max distance of %d from sensor at %A" p maxDistance sensor    
    let newState = { state with visited = Set.empty |> Set.add sensor }
    processNeighbor sensor maxDistance newState p

let processReading  (state:State) (reading:Reading) : State =

    printfn "\n\nProcessing sensor at %A and beacon at %A" reading.sensor reading.beacon

    let visited = Set.empty |> Set.add reading.sensor
    let cleared = state.cleared |> Set.add reading.sensor |> Set.add reading.beacon
    let updatedState = { state with visited = visited; cleared = cleared }

    let neighbors = getNeighbors reading.sensor
    printfn "Sensor %A neighbors are %A\n" reading.sensor neighbors
    let newState = neighbors
                    |> List.fold (processCloseNeighbor reading.sensor (reading.distance - 1)) updatedState

    newState

let solvePart1 (readings:Reading list) : State =

    let state = {
        visited = Set.empty;
        cleared = Set.empty;
    }

    let finalState = readings
                        |> List.fold processReading state

    finalState

let solve =
    // let lines = Common.getSampleDataAsArray 2022 15
    // let lines = Common.getChallengeDataAsArray 2022 15

    // let lines = [| "Sensor at x=5, y=5: closest beacon is at x=5, y=8" |]
    let lines = [| "Sensor at x=8, y=7: closest beacon is at x=2, y=10" |]
    printAllLines lines

    let readings = lines |> Seq.map parseLine |> List.ofSeq
    printfn "All readings: %A" readings
    
    let finalState = solvePart1 readings
    printfn "Final state"
    printf "%A" finalState

    printfn "\nWe cleared %d cells\n" finalState.cleared.Count

    for p in finalState.cleared |> Seq.sortBy GetY do
        printfn "Cleared: %A" p

    ()
