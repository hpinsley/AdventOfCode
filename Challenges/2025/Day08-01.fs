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


let buildDistancePairs (parsed: InCircuitBox array) : ((int * int) * float ) array =
    
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
                |> Seq.map (fun v ->
                                let (ic1, ic2) = fst v
                                let d = snd v
                                let b1 = fst ic1
                                let b2 = fst ic2

                                ((b1.boxId, b2.boxId), d)
                            )
                |> Array.ofSeq

let printDistanceCalc ((box1Id, box2Id), dist) : unit =
        printfn "Distance from %d to %d is %f"
                        box1Id
                        box2Id
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

let connectNetwork (network: Network) (((boxId1, boxId2), dist):((int * int ) * float)) : Network =

    let box1 = network.boxMap[boxId1]
    let box2 = network.boxMap[boxId2]
    let c1 = network.circuitMap[box1.circuitId]
    let c2 = network.circuitMap[box2.circuitId]

    // printfn "Connecting %d (%s) to %d (%s) with distance %f" box1.boxId (BoxLocString box1.location) box2.boxId (BoxLocString box2.location) dist

    if (box1.circuitId <> box2.circuitId)
    then
        // Combine networks
        let c1boxes = c1.junctionBoxIds
        let c2boxes = c2.junctionBoxIds
        let combinedBoxIds = Set.union c1boxes c2boxes
        let newCircuit = {Circuit.Factory() with junctionBoxIds = combinedBoxIds }
        let updatedCircuitMap = 
            network.circuitMap
                |> Map.remove box1.circuitId
                |> Map.remove box2.circuitId
                |> Map.add newCircuit.circuitId newCircuit

        let updatedBoxMap = 
            combinedBoxIds |>
                Set.fold (fun (m: Map<int, JunctionBox>) (boxId) -> 
                            Map.add boxId { m[boxId] with circuitId = newCircuit.circuitId } m
                         ) network.boxMap

        { network with boxMap = updatedBoxMap; circuitMap = updatedCircuitMap}

    else
        network


let computeScore (network: Network) : UInt64 =
    let sortedCircuits = network.circuitMap
                                    |> Map.values
                                    |> Seq.sortByDescending (fun c -> c.junctionBoxIds.Count)
                                    |> Array.ofSeq

    let score = sortedCircuits                                
                                    |> Array.take 3
                                    |> Array.fold (fun score c ->
                                                    score * (uint64) c.junctionBoxIds.Count
                                                ) 1UL
    score

let part1 (parsed: InCircuitBox array) (connectionsToMake: int) : unit =
    
    parsed |> Array.iter (fun cb -> printfn "Box %d loc: (%s)" (fst cb).boxId (BoxLocString (fst cb).location))
    printfn "There are %d junction boxes" parsed.Length

    let emptyNetwork = {
        boxMap = Map.empty; circuitMap = Map.empty
    }

    let nonInterconnectedNetwork = 
        parsed
            |> Array.fold addMeasuredBoxesToNetwork emptyNetwork

    let distanceCalcs = buildDistancePairs parsed
    printfn "Generated %d less pairs" distanceCalcs.Length
  
    // distanceCalcs |> Array.iter printDistanceCalc

    let network = distanceCalcs
                                |> Array.take connectionsToMake
                                |> Array.fold connectNetwork nonInterconnectedNetwork
           
    let score = computeScore network
    printfn "Final score is %ul" score
    ()


let solve =
    let stopWatch = Stopwatch.StartNew()

    // let lines = Common.getSampleDataAsArray 2025 8
    // let connectionsToMake = 10

    let lines: string array = Common.getChallengeDataAsArray 2025 8
    let connectionsToMake = 1000

    // printfn "%A" lines

    let parsed = parseInputData lines
    // printfn "%A" parsed

    part1 parsed connectionsToMake

    // printfn "First circuit id: %d" (getNextCircuitId())
    // printfn "Second circuit id: %d" (getNextCircuitId())
    

    ()