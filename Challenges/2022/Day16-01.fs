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

let printValve valve =
    printfn "Valve %s with flow %d has %d neighbors" valve.valveName valve.flowRate valve.neighbors.Length

let printValves (valves:Valve list) =
    for v in valves do
        printValve v

let rec getBestScore(fromValve:Valve) (stepsRemaining:int) (score:int) (valveOpen:Map<string, bool>) (visited:Set<string>) : int * Map<string, bool> * Set<string> =

    let allOpen = fun (valveOpen:Map<string,bool>) ->
        (valveOpen |> Map.filter (fun k v -> v) |> Seq.length) = valveOpen.Count
        
                                        
    //printfn "Looking for best from valve %s (flow rate %d).  Steps remaining %d; cum score = %d" fromValve.valveName fromValve.flowRate stepsRemaining score
    // if (stepsRemaining <= 0 || Set.contains fromValve.valveName visited)
    if (stepsRemaining <= 0 || allOpen valveOpen)
    then
        (score, valveOpen, visited)
    else
        // printfn "Visiting valve %s with %d steps remaining" fromValve.valveName stepsRemaining
        let nextVisited = Set.add fromValve.valveName visited
        let left = stepsRemaining - 1
        let isOpen = valveOpen[fromValve.valveName]

        if (not isOpen && fromValve.flowRate > 0)
        then
            // We have to see if we should open the valve, or leave it closed and choose the best neighbor

            // printfn "Opening valve %s with flow rate %d and %d remaining" fromValve.valveName fromValve.flowRate stepsRemaining
            let updatedMap = Map.add fromValve.valveName true valveOpen
            let myRelief = left * fromValve.flowRate
            let (openNeighorScore, openNeighborMap, openVisited) = fromValve.neighbors 
                                                                    |> Array.map (fun n -> getBestScore n (left-1) (score + myRelief) updatedMap nextVisited)
                                                                    |> Array.maxBy (fun (v, _, _) -> v)

            let (closeNeighborScore, closeNeighborMap, closeVisited) = fromValve.neighbors 
                                                                        |> Array.map (fun n -> getBestScore n (left) score valveOpen nextVisited)
                                                                        |> Array.maxBy (fun (v, _, _) -> v)

            let openScore = openNeighorScore
            let closeScore = closeNeighborScore

            if (openScore >= closeScore)
            then
                //printfn "Opening valve %s" fromValve.valveName
                (openScore, openNeighborMap, openVisited)
            else
                (closeScore, closeNeighborMap, closeVisited)

        else
            let neighborScores = fromValve.neighbors |> Array.map (fun n -> getBestScore n left score valveOpen nextVisited)
            let best = neighborScores |> Array.maxBy (fun (v, _, _) -> v)
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

    printfn "\nStarting valve"
    printValve startingValve

    let initialMap = valves |> List.map (fun v -> (v.valveName, false)) |> Map.ofList
    let initialSet = Set.empty

    let (best, _, _) = getBestScore startingValve 20 0 initialMap initialSet
    printfn "The best path relieves pressure of %A" best
    ()
