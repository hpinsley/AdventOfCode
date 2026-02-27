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
        JunctionBox.IdProvider.MoveNext() |> ignore
        JunctionBox.IdProvider.Current
        
and Circuit = 
    {
        CircuitId: string
        junctionBoxes: Set<JunctionBox>
    }
    static member IdProvider = (Seq.initInfinite id).GetEnumerator()
    static member GetNextId() : int =
        Circuit.IdProvider.MoveNext() |> ignore
        JunctionBox.IdProvider.Current


// let getNextCircuitId() : int =
//     static member CircuitIdProvider = (Seq.initInfinite id).GetEnumerator()

//     let _ = CircuitIdProvider.MoveNext()
//     CircuitIdProvider.Current
// let getNextBoxId() : int =
//     let _ = BoxIdProvider.MoveNext()
//     CircuitIdProvider.Current


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

let parseLine (line:string) : BOX_LOC =
    let splitData = line.Split(",")
    let result = { 
        x = UInt64.Parse(splitData[0]); 
        y = UInt64.Parse(splitData[1]); 
        z = UInt64.Parse(splitData[2]); 
    }
    result

let parseInputData (lines:string[]) : BOX_LOC[] =
    lines |> Array.map parseLine


let buildDistancePairs (parsed: BOX_LOC array) : ((BOX_LOC * BOX_LOC) * float) array =
    
    let n = parsed.Length

    seq { 0 .. n - 1}
                |> Seq.map (fun i ->
                                seq { i + 1 .. n - 1}
                                    |> Seq.map (fun j ->
                                                (parsed[i], parsed[j])
                                                )
                            )
                |> Seq.concat
                |> Seq.map (fun junctions -> (junctions, tuple_distance junctions))
                |> Seq.sortBy (fun v -> snd v)
                |> Array.ofSeq

let printDistanceCalc ((jb1, jb2),  dist) : unit =
        printfn "Distance from %s to %s is %f" 
                        (BoxLocString jb1)
                        (BoxLocString jb2)
                        dist

let part1 (parsed: BOX_LOC array) : unit =
    
    parsed |> Array.iter (fun jb -> printfn "Box loc: (%s)" (BoxLocString jb))
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