module Year2022Day16_Part1

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

let printValveInfo (valves:ValveInfo[]) =
    for v in valves do
        printfn "Valve %s with flow %d links to %A" v.valveName v.flowRate v.leadsTo

let printValve (valve:Valve) =
    printfn "Valve %s with flow %d has %d neighbors" valve.valveName valve.flowRate valve.neighbors.Length

let printValves (valves:Valve list) =
    for v in valves do
        printValve v

let rec getBestScore(fromValve:Valve) (stepsRemaining:int) (score:int) (valveOpen:Map<string, bool>) : int * Map<string, bool> =

    let allOpen = fun (valveOpen:Map<string,bool>) ->
        (valveOpen |> Map.filter (fun k v -> v) |> Seq.length) = valveOpen.Count
        
                                        
    //printfn "Looking for best from valve %s (flow rate %d).  Steps remaining %d; cum score = %d" fromValve.valveName fromValve.flowRate stepsRemaining score
    // if (stepsRemaining <= 0 || Set.contains fromValve.valveName visited)
    if (stepsRemaining <= 0 || allOpen valveOpen)
    then
        (score, valveOpen)
    else
        // printfn "Visiting valve %s with %d steps remaining" fromValve.valveName stepsRemaining
        let left = stepsRemaining - 1
        let isOpen = valveOpen[fromValve.valveName]

        if (not isOpen && fromValve.flowRate > 0)
        then
            // We have to see if we should open the valve, or leave it closed and choose the best neighbor

            // printfn "Opening valve %s with flow rate %d and %d remaining" fromValve.valveName fromValve.flowRate stepsRemaining
            let updatedMap = Map.add fromValve.valveName true valveOpen
            let myRelief = left * fromValve.flowRate
            let (openNeighorScore, openNeighborMap) = fromValve.neighbors 
                                                                    |> Array.map (fun n -> getBestScore n (left-1) (score + myRelief) updatedMap)
                                                                    |> Array.maxBy fst

            let (closeNeighborScore, closeNeighborMap) = fromValve.neighbors 
                                                                        |> Array.map (fun n -> getBestScore n (left) score valveOpen)
                                                                        |> Array.maxBy fst

            let openScore = openNeighorScore
            let closeScore = closeNeighborScore

            if (openScore >= closeScore)
            then
                //printfn "Opening valve %s" fromValve.valveName
                (openScore, openNeighborMap)
            else
                (closeScore, closeNeighborMap)

        else
            let neighborScores = fromValve.neighbors |> Array.map (fun n -> getBestScore n left score valveOpen)
            let best = neighborScores |> Array.maxBy fst
            best
    
let solve =
    let lines = Common.getSampleDataAsArray 2022 16
    // let lines = Common.getChallengeDataAsArray 2022 16

    //let lines = [|  "Valve AA has flow rate=0; tunnels lead to valves BB";
    //                "Valve BB has flow rate=13; tunnels lead to valves AA"
    //            |]

    printAllLines lines

    let valveInfo = lines |> Array.map parseLine
    let valves = parseValveInfo valveInfo
    printfn ""

    printValveInfo valveInfo
    printValves valves

    let startingValve = valves |> List.find (fun v -> v.valveName = valveInfo[0].valveName)
    let hyperValves = buildHyperValves valves startingValve
    printfn "%A" hyperValves


    //printfn "\nStarting valve"
    //printValve startingValve

    //let initialMap = valves |> List.map (fun v -> (v.valveName, false)) |> Map.ofList

    //let (best, _) = getBestScore startingValve 20 0 initialMap
    //printfn "The best path relieves pressure of %A" best
    ()
