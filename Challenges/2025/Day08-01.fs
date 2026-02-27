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

type JunctionBox = 
    {

        boxId: int
        location: BOX_LOC
        circuit: Circuit option
    }
    static member IdProvider = (Seq.initInfinite id).GetEnumerator()
    static member GetNextId() : int =
        let x = JunctionBox.IdProvider.MoveNext()
        printfn "%A" x
        JunctionBox.IdProvider.Current
    static member Factory (loc:BOX_LOC) : JunctionBox =
        let id = JunctionBox.GetNextId()
        { boxId = id; location = loc; circuit = Option.None }
        
and Circuit = 
    {
        CircuitId: int
        junctionBoxes: Set<JunctionBox>
    }
    static member IdProvider = (Seq.initInfinite id).GetEnumerator()
    static member GetNextId() : int =
        Circuit.IdProvider.MoveNext() |> ignore
        Circuit.IdProvider.Current
    static member Factory() : Circuit =
        let id = Circuit.GetNextId()
        { CircuitId = id; junctionBoxes = Set.empty }

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

let junctionDistance (boxes: JunctionBox * JunctionBox) : double =
    tuple_distance ((fst boxes).location, (snd boxes).location)

let parseLine (line:string) : JunctionBox =
    let splitData = line.Split(",")
    let result = { 
        x = UInt64.Parse(splitData[0]); 
        y = UInt64.Parse(splitData[1]); 
        z = UInt64.Parse(splitData[2]); 
    }
    JunctionBox.Factory result

let parseInputData (lines:string[]) : JunctionBox[] =
    lines |> Array.map parseLine


let buildDistancePairs (parsed: JunctionBox array) : ((JunctionBox * JunctionBox) * float) array =
    
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

let printDistanceCalc ((jb1: JunctionBox, jb2: JunctionBox),  dist) : unit =
        printfn "Distance from %d at  %s to %d at %s is %f"
                        jb1.boxId 
                        (BoxLocString jb1.location)
                        jb2.boxId
                        (BoxLocString jb2.location)
                        dist

let part1 (parsed: JunctionBox array) : unit =
    
    parsed |> Array.iter (fun jb -> printfn "Box %d loc: (%s)" jb.boxId (BoxLocString jb.location))
    printfn "There are %d junction boxes" parsed.Length
    let distanceCalcs = buildDistancePairs parsed
    printfn "Generated %d less pairs" distanceCalcs.Length
    
    distanceCalcs |> Array.iter printDistanceCalc


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