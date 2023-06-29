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
}

type State = {
    inventory: int[]
    factory: int[]
    mutable pendingRobot: int option
    mutable minute: int
}

let parseMaterial (s:string) : Material =
    match s with
        | "ore" -> Ore
        | "clay" -> Clay
        | "obsidian" -> Obsidian
        | "geode" -> Geode

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
            inventory = Array.create 4 0
            factory = Array.init 4 (fun m -> if m = Ore then 1 else 0)
            pendingRobot = None
            minute = 0
    }

    let states = Queue<State>()
    states.Enqueue initialState
    let mutable finishedStates = []

    let minuteCounter = Dictionary<int,int>()

    while (states.Count > 0) do
        if (states.Count % 10000 = 0)
        then
            printfn "State count = %d" states.Count


        let state = states.Dequeue()
        if (state.minute = MinuteLimit)
        then
            finishedStates <- state :: finishedStates
        else
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

            for m in seq { Ore .. Geode } do
                // Collect new inventory
                state.inventory[m] <- state.inventory[m] + state.factory[m]

            // Enqueue state with no robot creation
            states.Enqueue state

            // Create new pending robots
            for m in seq { Ore .. Geode } do
                let spec = bluePrint.robotSpecs[m]
                let potentalInventory = 
                    state.inventory
                        |> Array.mapi (fun i onHand -> onHand - spec.requires[i])
                if (potentalInventory |> Seq.exists (fun v -> v < 0) |> not)
                then
                    let altState = { state with
                                        inventory = potentalInventory
                                        factory = state.factory
                                        pendingRobot = None
                                   }
                    
                    states.Enqueue altState                                        

    let bestState = finishedStates |> List.maxBy (fun s -> s.inventory[Geode])
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

    let bluePrint = {
        planNumber = int m.Groups[1].Value
        robotSpecs = [| oreRobot ; clayRobot; obsidianRobot; geodeRobot |]
    }

    bluePrint
    
let solve =
    let lines = Common.getSampleDataAsArray 2022 19
    // let lines = Common.getChallengeDataAsArray 2022 19
    //printAllLines lines
    let plans = lines |> Array.map parseLine
    let bluePrint = plans[0]
    let result = optimizeBlueprint bluePrint
    printfn "Result: %A" result
    ()
