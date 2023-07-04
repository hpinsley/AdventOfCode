module Year2022Day19_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let MinuteLimit = 24

type Material = int

let Ore = 0
let Clay = 1
let Obsidian = 2
let Geode = 3

type RobotSpec = {
    manufactures: Material
    requires: int[]
}

type BluePrint = {
    planNumber: int
    robotSpecs: RobotSpec[]
    maxValues: int[]
}

type State = {
    stateId: int
    parentStateId: int
    inventory: int[]
    factory: int[]
    mutable pendingRobot: int option
    mutable minute: int
}

let mutable stateId = 0

let parseMaterial (s:string) : Material =
    match s with
        | "ore" -> Ore
        | "clay" -> Clay
        | "obsidian" -> Obsidian
        | "geode" -> Geode

let SkipThreshholds = [|
    //(22, [| 1; 4; 2; 2 |])
    //(21, [| 1; 4; 2; 1 |])
    //(19, [| 1; 4; 1; 1 |])
    //(14, [| 1; 4; 1; 0 |])
    //(6, [| 1; 2; 0; 0 |])
|]

let shouldSkipState(state:State) : bool =
    let threshhold = SkipThreshholds
                        |> Array.tryFind (fun (minute, _) -> state.minute = minute)
    match threshhold with
        | Some thresh ->
                let t = snd thresh

                let result = (state.factory[Ore] < t[Ore]
                                ||  state.factory[Clay] < t[Clay]
                                ||  state.factory[Obsidian] < t[Obsidian]
                                ||  state.factory[Geode] < t[Geode])
                if (result)
                then
                    ()
                result
        | None -> false

let makeRobotSpec (m:Match) (robotType:Material) (matchBase:int) : RobotSpec =
    let q1 = int m.Groups[matchBase].Value
    let m1 = parseMaterial m.Groups[matchBase + 1].Value

    let materials = 
        if (m.Groups[matchBase + 2].Success)
        then
            let q2 = int m.Groups[matchBase + 3].Value
            let m2 = parseMaterial m.Groups[matchBase + 4].Value
            [(m1, q1); (m2, q2)]
        else
            [(m1, q1)]
        
    let robotSpec = {
        manufactures = robotType
        requires = Array.create 4 0
    }
    
    for requirement in materials do
        robotSpec.requires[fst requirement] <- snd requirement

    robotSpec


// Create states with all possible robot spawning


let optimizeBlueprint (bluePrint:BluePrint) : int =

    let initialState = {
            stateId = 0
            parentStateId = -1
            inventory = Array.create 4 0
            factory = Array.init 4 (fun m -> if m = Ore then 1 else 0)
            pendingRobot = None
            minute = 0
    }

    let states = Queue<State>()
    states.Enqueue initialState
    let mutable finishedStates = []

    let minuteCounter = Dictionary<int,int>()
    let mostGeodeRobotsPerMinute = seq { 0 .. MinuteLimit }
                                    |> Seq.map (fun m -> KeyValuePair(m, 0))
                                    |> fun s -> new Dictionary<int, int>(s)

    let mutable skipCount = 0

    while (states.Count > 0) do
        if (states.Count % 1000000 = 0)
        then
            printfn "State count = %d" states.Count

        let state = states.Dequeue()
        if (state.minute = MinuteLimit)
        then
            finishedStates <- state :: finishedStates
        elif (shouldSkipState state)
        then
            skipCount <- skipCount + 1
            if (skipCount % 1 = 0)
            then
                printfn "Skipped %d states based on hardcoded" skipCount
        else
            // See if there are state with more geode robots for the same minute
            let geodeRobots = state.inventory[Geode]
            let maxGeodeRobots = mostGeodeRobotsPerMinute[state.minute]
            if (geodeRobots < maxGeodeRobots)
            then
                skipCount <- skipCount + 1
                if (skipCount % 10000000 = 0)
                then
                    printfn "Skipped %d states based on GEODE MAX" skipCount
            else
                if (geodeRobots > maxGeodeRobots)
                then
                    mostGeodeRobotsPerMinute[state.minute] <- geodeRobots

                // Advance to next minute
                state.minute <- state.minute + 1

                if (minuteCounter.ContainsKey(state.minute))
                then
                    minuteCounter[state.minute] <- minuteCounter[state.minute] + 1
                else
                    printfn "Starting minute %d" state.minute
                    minuteCounter[state.minute] <- 1

                // If we have a pending robot, put it into service in our factory
                match state.pendingRobot with
                                | Some m -> state.factory[m] <- state.factory[m] + 1
                                            state.pendingRobot <- None
                                | None -> ()
                    |> ignore

                let newInventory = state.inventory |> Array.mapi (fun i v -> v + state.factory[i])

                //for m in seq { Ore .. Geode } do
                //    // Collect new inventory
                //    state.inventory[m] <- state.inventory[m] + state.factory[m]

                // Create new pending robots
                let mutable createCount = 0

                for m in seq { Ore .. Geode } do

                    //Note that we can do a bit better: For any resource R that's not geode: if you already have X robots creating resource R, 
                    //a current stock of Y for that resource, T minutes left, and no robot requires more than Z of resource R 
                    //to build, and X * T+Y >= T * Z, then you never need to build another robot mining R anymore.
                    let x = state.factory[m]
                    let y = state.inventory[m]
                    let t = MinuteLimit - state.minute
                    let z = bluePrint.maxValues[m]
                    let v1 = x * t + y
                    let v2 = t * z
                    if ((m <> Geode) && (v1 > v2))
                    then
                        skipCount <- skipCount + 1
                        if (skipCount % 100000 = 0)
                        then
                            printfn "Skipped %d states based on max values" skipCount
                    else
                        let spec = bluePrint.robotSpecs[m]
                        let potentalInventory = 
                            state.inventory
                                |> Array.mapi (fun i onHand -> onHand - spec.requires[i])
                        if (potentalInventory |> Seq.exists (fun v -> v < 0) |> not)
                        then
                            stateId <- stateId + 1
                            let adjustedInventory = newInventory
                                                        |> Array.mapi (fun i onHand -> onHand - spec.requires[i])
                            let altState = { state with
                                                stateId = stateId
                                                parentStateId = state.stateId
                                                inventory = adjustedInventory
                                                factory = state.factory |> Array.copy
                                                pendingRobot = Some m
                                            }
                    
                            states.Enqueue altState
                            createCount <- createCount + 1

                // Enqueue state with no robot creation
                //if (createCount = 0)
                //then
                states.Enqueue { state with inventory = newInventory }

    let bestState = finishedStates |> List.maxBy (fun s -> s.inventory[Geode])
    //let bestStates = finishedStates |> List.filter (fun s -> s.inventory[Geode] = bestState.inventory[Geode])
    bestState.inventory[Geode]

let parseLine (line:string) : BluePrint =

    let pattern = "Blueprint (\d+): Each ore robot costs (\d+) (\w+)( and (\d+) (\w+))?\. Each clay robot costs (\d+) (\w+)( and (\d+) (\w+))?\. Each obsidian robot costs (\d+) (\w+)( and (\d+) (\w+))?\. Each geode robot costs (\d+) (\w+)( and (\d+) (\w+))?\."

    let m = Regex.Match(line, pattern)
    if (not m.Success)
    then
        failwith "No match"

    let oreRobot = makeRobotSpec m Ore 2
    let clayRobot = makeRobotSpec m Clay 7
    let obsidianRobot = makeRobotSpec m Obsidian 12
    let geodeRobot = makeRobotSpec m Geode 17
    let robotSpecs = [| oreRobot ; clayRobot; obsidianRobot; geodeRobot |]
    let maxValues = robotSpecs |> Array.fold (fun maxValues spec -> 
                                                                [|  max spec.requires[Ore] maxValues[Ore];
                                                                    max spec.requires[Clay] maxValues[Clay];
                                                                    max spec.requires[Obsidian] maxValues[Obsidian];
                                                                    max spec.requires[Geode] maxValues[Geode];
                                                                |]
                                                ) [| 0; 0; 0; 0 |]
                    
    let bluePrint = {
        planNumber = int m.Groups[1].Value
        robotSpecs = robotSpecs
        maxValues = maxValues
    }

    bluePrint
    
let solve =
    printfn "Solve has been called"
    //let lines = Common.getSampleDataAsArray 2022 19
    let lines = Common.getChallengeDataAsArray 2022 19
    //printAllLines lines
    let plans = lines |> Array.map parseLine
    //let bluePrint = plans[0]
    //let result = optimizeBlueprint bluePrint
    //printfn "Result: %A" result

    let totalScore = plans
                        |> Array.fold (fun s p -> 
                                           printfn "Starting plan %d" p.planNumber
                                           let geodes = optimizeBlueprint p
                                           let planScore = p.planNumber * geodes
                                           s + planScore
                                    ) 0
    
    printfn "The total score is %d" totalScore
    ()
