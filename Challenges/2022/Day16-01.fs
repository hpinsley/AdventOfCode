﻿module Year2022Day16_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let third = fun (_,_,v) -> v

type ValveInfo =
    {
        valveName: string;
        flowRate: int;
        leadsTo: string[]
    }

type Valve =
    {
        valveName: string;
        mutable flowRate: int;
        mutable neighbors: Valve[]
    }

type DistantNeighbor = {
    valveName: string;
    distance: int;
}

type HyperValve =
    {
        valveName: string;
        flowRate: int;
        neighbors: DistantNeighbor[];
    }

let buildHyperValves (valves:Valve list) (startingValve:Valve): HyperValve list =
    let rec getDistantNeighbors (valve:Valve) (distance:int) (toIgnore:Set<string>) : DistantNeighbor[] =
        let nextDoor = valve.neighbors |> Array.filter (fun v -> not (toIgnore.Contains v.valveName)) |> Array.filter (fun v -> v.flowRate > 0) |> Array.map (fun v -> { valveName = v.valveName; distance = distance })
        let brokenNeighbors = valve.neighbors |> Array.filter (fun v -> not (toIgnore.Contains v.valveName)) |> Array.filter (fun v -> v.flowRate = 0)
        let toIgnoreClose = nextDoor |> Array.map (fun v -> v.valveName) |> Set.ofArray |> Set.add valve.valveName
        let toIgnoreUpdated = Set.union toIgnore toIgnoreClose
        let reachable = brokenNeighbors |> Array.map (fun v -> getDistantNeighbors v (distance + 1) toIgnoreUpdated) |> Array.concat
        reachable |> Array.append nextDoor

    let initialList = valves
                            |> List.map (fun v -> 
                                                let reachable = getDistantNeighbors v 1 Set.empty
                            
                                                {
                                                    valveName = v.valveName;
                                                    flowRate = v.flowRate;
                                                    neighbors = reachable;
                                                }
                                        )
    let prunedList = initialList |> List.filter (fun hv -> hv.valveName = startingValve.valveName || hv.flowRate > 0)
    prunedList

let parseLine (line:string) : ValveInfo =
    let pattern = "Valve (..) has flow rate=(\d+); tunnel(s)? lead(s)? to valve(s)? (.*)"
    let m = Regex(pattern).Match(line)
    if (not m.Success) then failwith "regex"

    let targetList = (m.Groups[6].Value).Replace(" ", "")
    let targets = targetList.Split(',')
    let info = {
        valveName = m.Groups[1].Value;
        flowRate = int m.Groups[2].Value
        leadsTo = targets
    }
    info

let parseValveInfo (valves:ValveInfo[]) : Valve list =
    let valveDict = new Dictionary<string, Valve>()

    let getValve (k:string) = if (valveDict.ContainsKey(k))
                                then 
                                    valveDict[k]
                                else
                                    let (v:Valve) = {
                                            valveName = k;
                                            flowRate = 0;
                                            neighbors = [||];
                                        }
                                    valveDict[k] <- v
                                    v

    for vi in valves do
        let v = getValve vi.valveName
        v.flowRate <- vi.flowRate
        v.neighbors <- vi.leadsTo |> Array.map (fun s -> getValve s)
    
    valveDict.Values |> List.ofSeq

let fst3 (v, _, _) =
    v

let printValveInfo (valves:ValveInfo[]) =
    for v in valves do
        printfn "Valve %s with flow %d links to %A" v.valveName v.flowRate v.leadsTo

let printValves (valves:Valve list) =
    for valve in valves do
        printfn "Valve %s with flow %d has %d neighbors" valve.valveName valve.flowRate valve.neighbors.Length

let printHyperValves (valves:HyperValve seq) =
    for valve in valves do
        printfn "Valve %s with flow %d has %d neighbors" valve.valveName valve.flowRate valve.neighbors.Length
        for dn in valve.neighbors do
            printfn "\t%s (%d)" dn.valveName dn.distance

let CallCache = new Dictionary<(string * int * int * Set<string>),(int * Set<string> * int)>();

let rec getBestScore (valveMap:Map<string, HyperValve>) (currentValve:HyperValve) (stepsRemaining:int) (score:int) (openValves:Set<string>) (ancestors:string) : (int * Set<string> * int) =

    let rec getBestScoreInternal (valveMap:Map<string, HyperValve>) (currentValve:HyperValve) (stepsRemaining:int) (score:int) (openValves:Set<string>) (ancestors:string) : (int * Set<string> * int) =

        let prefix = ancestors // Save time; do we really need this.....   + ";" + currentValve.valveName
        //printfn "Score: %d %s (open are %s)" score prefix (String.Join(',', openValves))
        //printfn "There are %d valves in our list and %d are open" valveMap.Count openValves.Count
        if (stepsRemaining <= 0)
        then
            (score, openValves, stepsRemaining)
        else
            // printfn "Visiting valve %s with %d steps remaining" fromValve.valveName stepsRemaining
            let isOpen = openValves |> Set.contains currentValve.valveName

            if (not isOpen && currentValve.flowRate > 0)
            then
                // We have to see if we should open the valve, or leave it closed and choose the best neighbor

                // printfn "Opening valve %s with flow rate %d and %d remaining" fromValve.valveName fromValve.flowRate stepsRemaining
                let updatedSetOfOpenValves = Set.add currentValve.valveName openValves
                let myRelief = (stepsRemaining - 1) * currentValve.flowRate
                let neighborScoresIfWeOpen = currentValve.neighbors
                                                                |> Array.filter (fun dn -> dn.distance < (stepsRemaining - 1))
                                                                |> Array.map (fun n -> (valveMap[n.valveName], n.distance))
                                                                |> Array.map (fun (n,d) -> getBestScore valveMap n ((stepsRemaining - 1) - d) (score + myRelief) updatedSetOfOpenValves prefix)
                let (openScore, openNeighborMap, openRemaining) = 
                    if (Array.isEmpty neighborScoresIfWeOpen)
                    then
                        (score + myRelief, updatedSetOfOpenValves, stepsRemaining)
                    else
                        Array.maxBy fst3 neighborScoresIfWeOpen
            
            
                let neighborScoresIfWeClose = currentValve.neighbors 
                                                                |> Array.filter (fun dn -> dn.distance < stepsRemaining)
                                                                |> Array.map (fun n -> (valveMap[n.valveName], n.distance))
                                                                |> Array.map (fun (n,d) -> getBestScore valveMap n (stepsRemaining - d) score openValves prefix)

                let (closeScore, closeNeighborMap, closedRemaining) =
                    if (Array.isEmpty neighborScoresIfWeClose)
                    then
                        (score, openValves, stepsRemaining - 1)
                    else
                        Array.maxBy fst3 neighborScoresIfWeClose

                if (openScore >= closeScore)
                then
                    //printfn "Opening valve %s" fromValve.valveName
                    (openScore, openNeighborMap, openRemaining)
                else
                    (closeScore, closeNeighborMap, closedRemaining)

            else
                let neighborScores = currentValve.neighbors
                                        |> Array.filter (fun dn -> dn.distance < stepsRemaining)
                                        |> Array.map (fun n -> (valveMap[n.valveName], n.distance))
                                        |> Array.map (fun (n,d) -> getBestScore valveMap n (stepsRemaining - d) score openValves prefix)
            
                if (Array.isEmpty neighborScores)
                then
                    (score, openValves, stepsRemaining - 1)
                else
                    let best = neighborScores |> Array.maxBy fst3
                    best

    // Outer function here
    let key = (currentValve.valveName, stepsRemaining, score, openValves)
    if (CallCache.ContainsKey(key))
    then
        // printfn "Returning %A value from cache!" key
        CallCache[key]
    else
        let result = getBestScoreInternal valveMap currentValve stepsRemaining score openValves ancestors
        CallCache[key] <- result
        result

let solveHyperValves (valves:HyperValve seq) : int =
    let timeRemaining = 30
    let valveMap = valves |> Seq.map (fun v -> (v.valveName, v)) |> Map.ofSeq
    let startingValve = valves |> Seq.find (fun v -> v.flowRate = 0)
    let openValves = Set.empty
    let (score, opened, remaining) = getBestScore valveMap startingValve timeRemaining 0 openValves ""
    score

let solve =
    // let lines = Common.getSampleDataAsArray 2022 16
    let lines = Common.getChallengeDataAsArray 2022 16

    //let lines = [|  "Valve AA has flow rate=0; tunnels lead to valves BB";
    //                "Valve BB has flow rate=13; tunnels lead to valves AA"
    //            |]

    printAllLines lines

    let valveInfo = lines |> Array.map parseLine
    let valves = parseValveInfo valveInfo
    printfn ""

    //printValveInfo valveInfo
    //printValves valves

    let startingValve = valves |> List.find (fun v -> v.valveName = "AA")
    let hyperValves = buildHyperValves valves startingValve
    printHyperValves hyperValves

    let result = solveHyperValves hyperValves
    printfn "Result is %d" result

    ()
