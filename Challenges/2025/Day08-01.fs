module Year2025Day8_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

type DISTANCE = uint64

type BOX_LOC = {
    x: DISTANCE
    y: DISTANCE
    z: DISTANCE
}

let mutable private nextJunctionBoxId = 0
let mutable private nextCircuitId = 0

type JunctionBox =
    {
        boxId: int
        location: BOX_LOC
        circuitId: int
        linksTo: (int * int) option
    }
    static member GetNextId() : int =
        let id = nextJunctionBoxId
        nextJunctionBoxId <- nextJunctionBoxId + 1
        id
    static member Factory (loc:BOX_LOC) : JunctionBox * Circuit =
        let newBoxId = JunctionBox.GetNextId()
        let emptyCircuit = Circuit.Factory()
        let newBox = { boxId = newBoxId; location = loc; circuitId = emptyCircuit.circuitId; linksTo = Option.None }
        let singleBoxCircuit = { emptyCircuit with junctionBoxIds = Set.add newBoxId Set.empty }
        (newBox, singleBoxCircuit)

and Circuit =
    {
        circuitId: int
        junctionBoxIds: Set<int>
    }
    static member GetNextId() : int =
        let id = nextCircuitId
        nextCircuitId <- nextCircuitId + 1
        id
    static member Factory() : Circuit =
        let id = Circuit.GetNextId()
        { circuitId = id; junctionBoxIds = Set.empty }

and InCircuitBox = JunctionBox * Circuit
and DistanceCalc = (InCircuitBox * InCircuitBox) * float


let BoxLocString (bl:BOX_LOC) : string =
    sprintf "%5d, %5d, %5d" bl.x bl.y bl.z


let distance (b1:BOX_LOC) (b2:BOX_LOC) : double =
    let dx = (double) b2.x - (double) b1.x
    let dy = (double) b2.y - (double) b1.y
    let dz = (double) b2.z - (double) b1.z

    let dist = sqrt(dx**2 + dy**2 + dz**2)
    dist

let tuple_distance (t: BOX_LOC * BOX_LOC) : double =
    distance (fst t) (snd t)

let junctionDistance (circuitBoxes: InCircuitBox * InCircuitBox) : double =
    let (cb1, cb2) = circuitBoxes
    let (box1, box2) = (fst cb1, fst cb2)
    tuple_distance (box1.location, box2.location)

let parseLine (line:string) : InCircuitBox =
    let splitData = line.Split(",")
    let box_loc = { 
        x = UInt64.Parse(splitData[0]); 
        y = UInt64.Parse(splitData[1]); 
        z = UInt64.Parse(splitData[2]); 
    }
    JunctionBox.Factory box_loc

let parseInputData (lines:string[]) : InCircuitBox[] =
    lines |> Array.map parseLine


let buildDistancePairs (parsed: InCircuitBox array) : DistanceCalc array =
    
    let n = parsed.Length

    seq { 0 .. n - 1}
                |> Seq.map (fun i ->
                                seq { i + 1 .. n - 1}
                                    |> Seq.map (fun j ->
                                                (parsed[i], parsed[j])
                                                )
                            )
                |> Seq.concat
                |> Seq.map (fun junctions -> (junctions, junctionDistance junctions))
                |> Seq.sortBy (fun v -> snd v)
                |> Array.ofSeq

let printDistanceCalc (distanceCalc: DistanceCalc) : unit =
        let ((cb1, cb2), dist) = distanceCalc
        let (jb1, jb2) = (fst cb1, fst cb2)

        printfn "Distance from %d at  %s to %d at %s is %f"
                        jb1.boxId 
                        (BoxLocString jb1.location)
                        jb2.boxId
                        (BoxLocString jb2.location)
                        dist


type Network = {
    boxMap: Map<int, JunctionBox>
    circuitMap: Map<int, Circuit>
}

let addMeasuredBoxesToNetwork (network: Network) (dc: InCircuitBox) : Network =
    let (jb, c) = dc

    { network with 
        boxMap = Map.add jb.boxId jb network.boxMap;
        circuitMap = Map.add c.circuitId c network.circuitMap
    }

let part1 (parsed: InCircuitBox array) : unit =
    
    parsed |> Array.iter (fun cb -> printfn "Box %d loc: (%s)" (fst cb).boxId (BoxLocString (fst cb).location))
    printfn "There are %d junction boxes" parsed.Length

    let emptyNetwork = {
        boxMap = Map.empty; circuitMap = Map.empty
    }

    let network = parsed
                        |> Array.fold addMeasuredBoxesToNetwork emptyNetwork



    let distanceCalcs = buildDistancePairs parsed
    printfn "Generated %d less pairs" distanceCalcs.Length
  
    distanceCalcs |> Array.iter printDistanceCalc

    ()


let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2025 8
    // let lines: string array = Common.getChallengeDataAsArray 2025 8
    // printfn "%A" lines

    let parsed = parseInputData lines
    printfn "%A" parsed

    part1 parsed

    // printfn "First circuit id: %d" (getNextCircuitId())
    // printfn "Second circuit id: %d" (getNextCircuitId())
    

    ()