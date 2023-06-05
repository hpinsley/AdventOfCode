module Year2022Day16_Part2

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Threading.Tasks
open System.Collections.Concurrent
open System.Threading


let locker = ref 0

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
    let count = valves |> Seq.length
    let countOfGoodValves = valves |> Seq.filter (fun v -> v.flowRate > 0) |> Seq.length
    printfn "There are %d valves, %d of which are good ones" count countOfGoodValves

let mutable CallCache = new ConcurrentDictionary<(Set<string> * string * int * int * Set<string>),(int * Set<string> * int)>();

let rec getBestScore (recCount:int) (allowedToOpen:Set<string>) (valveMap:Map<string, HyperValve>) (currentValve:HyperValve) (stepsRemaining:int) (score:int) (openValves:Set<string>) : (int * Set<string> * int) =
    let useCache = true

    if (not useCache)
    then
        getBestScoreInternal recCount allowedToOpen valveMap currentValve stepsRemaining score openValves
    else
        let key = (allowedToOpen, currentValve.valveName, stepsRemaining, score, openValves)
    
        let (found, v) = CallCache.TryGetValue(key)
        if (found) then
            v
        else
            let result = getBestScoreInternal recCount allowedToOpen valveMap currentValve stepsRemaining score openValves
            CallCache.AddOrUpdate(key, result, fun k v -> result) |> ignore
            //CallCache[key] <- result
            result

and getBestScoreInternal (recCount:int) (allowedToOpen) (valveMap:Map<string, HyperValve>) (currentValve:HyperValve) (stepsRemaining:int) (score:int) (openValves:Set<string>) : (int * Set<string> * int) =

    //printfn "Score: %d %s (open are %s)" score prefix (String.Join(',', openValves))
    //printfn "There are %d valves in our list and %d are open" valveMap.Count openValves.Count
    if (stepsRemaining <= 0)
    then
        (score, openValves, stepsRemaining)
    else
        // printfn "Visiting valve %s with %d steps remaining" fromValve.valveName stepsRemaining
        let isOpen = openValves |> Set.contains currentValve.valveName

        if (not isOpen && currentValve.flowRate > 0 && Set.contains currentValve.valveName allowedToOpen)
        then
            // We have to see if we should open the valve, or leave it closed and choose the best neighbor

            // printfn "Opening valve %s with flow rate %d and %d remaining" fromValve.valveName fromValve.flowRate stepsRemaining
            let updatedSetOfOpenValves = Set.add currentValve.valveName openValves
            let myRelief = (stepsRemaining - 1) * currentValve.flowRate
            let neighborScoresIfWeOpen = currentValve.neighbors
                                                            |> Array.filter (fun dn -> dn.distance < (stepsRemaining - 1))
                                                            |> Array.map (fun n -> (valveMap[n.valveName], n.distance))
                                                            |> Array.map (fun (n,d) ->
                                                                                //printfn "OPEN SECTION Recursing (if we open) (thread %d)" Thread.CurrentThread.ManagedThreadId
                                                                                getBestScore (recCount + 1) allowedToOpen valveMap n ((stepsRemaining - 1) - d) (score + myRelief) updatedSetOfOpenValves)
            //printfn "OPEN SECTION Got neighborScoresIfWeOpen  (thread %d)" Thread.CurrentThread.ManagedThreadId
            let (openScore, openNeighborMap, openRemaining) = 
                if (Array.isEmpty neighborScoresIfWeOpen)
                then
                    (score + myRelief, updatedSetOfOpenValves, stepsRemaining)
                else
                    Array.maxBy fst3 neighborScoresIfWeOpen
            
            
            let neighborScoresIfWeClose = currentValve.neighbors 
                                                            |> Array.filter (fun dn -> dn.distance < stepsRemaining)
                                                            |> Array.map (fun n -> (valveMap[n.valveName], n.distance))
                                                            |> Array.map (fun (n,d) -> 
                                                                                    //printfn "Recursing (if we DONT open) (thread %d)" Thread.CurrentThread.ManagedThreadId
                                                                                    getBestScore (recCount + 1) allowedToOpen valveMap n (stepsRemaining - d) score openValves)
            //printfn "OPEN SECTION Got neighborScoresIfWeClose  (thread %d)" Thread.CurrentThread.ManagedThreadId
                
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

        else    // This appears to be where it normally hangs
            let neighborScores = currentValve.neighbors
                                    |> Array.filter (fun dn -> dn.distance < stepsRemaining)
                                    |> Array.map (fun n -> (valveMap[n.valveName], n.distance))
                                    |> Array.map (fun (n,d) ->
                                        //printfn "CLOSED SECTION Recursing without UNOPEN (thread %d)" Thread.CurrentThread.ManagedThreadId
                                        getBestScore (recCount + 1) allowedToOpen valveMap n (stepsRemaining - d) score openValves)
            //printfn "CLOSED SECTION Got neighbor scores UNOPEN section (thread %d)" Thread.CurrentThread.ManagedThreadId
            if (Array.isEmpty neighborScores)
            then
                (score, openValves, stepsRemaining - 1)
            else
                let best = neighborScores |> Array.maxBy fst3
                best


let getCombosToTry (valves:string list) (minNumberPerWorker:int) : (string list * string list) list =
    let permutedIndices = allSplits valves.Length
    let allCombos = permutedIndices
                    |> List.map (fun (set1, set2) ->
                                    (
                                         set1 |> List.map (fun i -> valves[i])
                                       , set2 |> List.map (fun i -> valves[i])
                                    )
                                )
    allCombos |> List.filter (fun (set1, set2) -> set1.Length >= minNumberPerWorker && set2.Length >= minNumberPerWorker)

let solveHyperValves (valves:HyperValve seq) : int =
    // let timeRemaining = 26
    let timeRemaining = 5
    let valveMap = valves |> Seq.map (fun v -> (v.valveName, v)) |> Map.ofSeq
    let startingValve = valves |> Seq.find (fun v -> v.flowRate = 0)
    let openValves = Set.empty

    
    let goodValves = valves
                            |> Seq.filter (fun v -> v.flowRate > 0)
                            |> Seq.map (fun v -> v.valveName) 
                            |> List.ofSeq

    let minValvesAssignedPerWorker = goodValves.Length / 2
    let attempts = getCombosToTry goodValves minValvesAssignedPerWorker
    printfn "%A" attempts
    printfn "Number of valves: %d, permuations: %d" goodValves.Length attempts.Length
    let mutable best = Int32.MinValue

    let mutable options = new ParallelOptions()
    options.MaxDegreeOfParallelism <- -1
    //options.TaskScheduler <- TaskScheduler.Default
    let attemptsArray = attempts |> Array.ofList // |> Array.take 1

    let parallelLoopResult = Parallel.For(0, attemptsArray.Length, 
                                            options,
                                            fun (i:int) (p:ParallelLoopState) -> 
                                                printfn "Parallel Loop state for %d on thread %d has exception flag %A and isStopped %A" 
                                                            i
                                                            Thread.CurrentThread.ManagedThreadId
                                                            p.IsExceptional
                                                            p.IsStopped

                                                let split = attemptsArray[i]
                                                try
                                                    printfn "In TRY %d on thread %d" i System.Threading.Thread.CurrentThread.ManagedThreadId
 
                                                    let humanCanOpen = fst split |> Set.ofSeq
                                                    let elephantCanOpen = snd split |> Set.ofSeq
                                                    let (humanScore, _, _) = getBestScore 0 humanCanOpen valveMap startingValve timeRemaining 0 openValves
                                                    let (elephantScore, _, _) = getBestScore 0 elephantCanOpen valveMap startingValve timeRemaining 0 openValves
                                                    let totalScore = humanScore + elephantScore
                                                    // let totalScore = humanScore
                                                    if (totalScore > best)
                                                    then
                                                        printfn "Updating best to %d on thread %d" totalScore Thread.CurrentThread.ManagedThreadId
                                                        best <- totalScore
                                                        printfn "Updated best to %d on thread %d" best Thread.CurrentThread.ManagedThreadId

                                                    //lock locker (fun () ->
                                                    //                printfn "Got lock in %d on thread %d" i Thread.CurrentThread.ManagedThreadId
                                                    //                results <- totalScore :: results
                                                    //                printfn "Stored results in %d on thread %d" i Thread.CurrentThread.ManagedThreadId
                                                    //              )
                                                    //printfn "Released lock for %d on thread %d" i Thread.CurrentThread.ManagedThreadId
                                                    ()
                                                with ex  ->
                                                    printfn "error: %s" ex.Message
                                                    )

    printfn "ParallelLoopResult = %A" parallelLoopResult

    printfn "The best is %d" best
    
    //for i in seq { 0 .. (attempts.Length - 1)} do
    //    if i % 10 = 0 then
    //        printfn "Iteration %d" i
    //    let humanCanOpen = fst attempts[i] |> Set.ofSeq
    //    let elephantCanOpen = snd attempts[i] |> Set.ofSeq
    //    let (humanScore, _, _) = getBestScore humanCanOpen valveMap startingValve timeRemaining 0 openValves
    //    let (elephantScore, _, _) = getBestScore elephantCanOpen valveMap startingValve timeRemaining 0 openValves
    //    let totalScore = humanScore + elephantScore
    //    results <- totalScore :: results
    
    best

//let solveHyperValvesAltThread (valves:HyperValve seq) : int =
//    let task = System.Threading.Tasks.Task.Factory.StartNew (fun () ->
//                                                                printfn "In Task with thread %d" System.Threading.Thread.CurrentThread.ManagedThreadId
//                                                                let result = solveHyperValves valves
//                                                                printfn "Final result is %A" result
//                                                             )
//    task.Wait()
//    0

let solve =
    printfn "Solve has been called"
    let lines = Common.getSampleDataAsArray 2022 16
    // let lines = Common.getChallengeDataAsArray 2022 16

    printAllLines lines

    let valveInfo = lines |> Array.map parseLine
    let valves = parseValveInfo valveInfo
    printfn ""

    let startingValve = valves |> List.find (fun v -> v.valveName = "AA")
    let hyperValves = buildHyperValves valves startingValve
    printHyperValves hyperValves

    //let result = solveHyperValves (hyperValves |> Array.ofList)
    //printfn "Part 2 Result is %d" result
    let t = Task.Factory.StartNew(fun () -> 
                                        let result = solveHyperValves (hyperValves |> Array.ofList)
                                        printfn "Part 2 Result is %d" result
                                 )

    printfn "Waiting..."
    t.Wait()
    printfn "Done"

