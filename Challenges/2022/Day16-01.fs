module Year2022Day16_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type ValveInfo =
    {
        valveName: string;
        flowRate: int;
        leadsTo: string[]
    }

type ValveState =
    | Open
    | Closed

type Valve =
    {
        valveName: string;
        mutable flowRate: int;
        mutable neighbors: Valve[]
        mutable valveState: ValveState        
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
                                            valveState = Closed
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
    printfn "%s Valve %s with flow %d has %d neighbors" (if valve.valveState = Open then "Opened" else "Closed")  valve.valveName valve.flowRate valve.neighbors.Length

let printValves (valves:Valve list) =
    for v in valves do
        printValve v

let rec getBestScore(fromValve:Valve) (stepsRemaining:int) (score:int) (visited:Set<string>) : int * Set<string> =

    if (false && Set.contains fromValve.valveName visited)
    then
        (score, visited)
    else
        let newVisited = Set.add fromValve.valveName visited
        printfn "Looking for best from valve %s (flow rate %d).  Steps remaining %d; cum score = %d" fromValve.valveName fromValve.flowRate stepsRemaining score
        if (stepsRemaining <= 1)
        then
            (score, newVisited)
        else
            // printfn "Visiting valve %s with %d steps remaining" fromValve.valveName stepsRemaining
            let left = stepsRemaining - 1

            if (fromValve.valveState = Closed && fromValve.flowRate > 0)
            then
                printfn "Opening valve %s with flow rate %d and %d remaining" fromValve.valveName fromValve.flowRate stepsRemaining
                fromValve.valveState <- Open
                let openTheValveScore = getBestScore fromValve left (score + left * fromValve.flowRate) newVisited
                let neighborScores = if (left = 0) then Array.empty else fromValve.neighbors |> Array.map (fun n -> getBestScore n left score newVisited)
                let allChoices = Array.append neighborScores [| openTheValveScore |]
                let best = allChoices |> Array.maxBy fst
                best
            else
                if (left = 1)
                then
                    (score, newVisited)
                else
                    let neighborScores = fromValve.neighbors |> Array.map (fun n -> getBestScore n left score newVisited)
                    let best = neighborScores |> Array.maxBy fst
                    best

    
let solve =
    let lines = Common.getSampleDataAsArray 2022 16
    // let lines = Common.getChallengeDataAsArray 2022 16
    printAllLines lines

    let valveInfo = lines |> Array.map parseLine
    let valves = parseValveInfo valveInfo
    printfn ""

    printValveInfo valveInfo
    printValves valves

    let startingValve = valves |> List.find (fun v -> v.valveName = valveInfo[0].valveName)

    printfn "\nStarting valve"
    printValve startingValve

    let best = getBestScore startingValve 30 0 (Set.empty)
    printfn "The best path relieves pressure of %A" best
    ()
