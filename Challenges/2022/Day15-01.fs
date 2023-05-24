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
    pendingReadings: Reading list;
}

let GetX = fst
let GetY = snd

let getDistance (p1:Point) (p2:Point) : int =
    let x1 = GetX p1
    let x2 = GetX p2
    let y1 = GetY p1
    let y2 = GetY p2

    Math.Abs(x2 - x1) + Math.Abs(y2 - y1)

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

let processReading  (state:State) (reading:Reading) : State =
    { state with visited = Set.add reading.sensor state.visited }

let rec processReadings  (state:State) (reading:Reading) : State =
    match state.pendingReadings with
        | [] -> state
        | reading :: remaining ->
            let newState = processReading { state with pendingReadings = remaining } reading
            remaining |> List.fold processReadings newState

let solvePart1 (readings:Reading list) : State =

    let state = {
        visited = Set.empty;
        cleared = Set.empty;
        pendingReadings = readings;
    }

    let finalState = readings
                        |> List.fold processReadings state

    finalState

let solve =
    let lines = Common.getSampleDataAsArray 2022 15
    // let lines = Common.getChallengeDataAsArray 2022 15
    printAllLines lines

    let readings = lines |> Seq.map parseLine |> List.ofSeq
    printfn "%A" readings
    
    let finalState = solvePart1 readings
    printfn "Final state"
    printf "%A" finalState

    ()
