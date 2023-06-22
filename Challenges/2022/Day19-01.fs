module Year2022Day19_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let MinuteLimit = 24

// Define trimming points (minute, min geode)
let Threshholds = [(19, 1)]

type Material =
    | Ore
    | Clay
    | Obsidian
    | Geode

let MinRobots = [16, 
                    [(Ore, 1); (Clay, 4); (Obsidian, 2); (Geode, 0)]
                ]
let getSuccessorMaterial (fromMaterial:Material) : Material option =
    match fromMaterial with
        | Ore -> Some Clay
        | Clay -> Some Obsidian
        | Obsidian -> Some Geode
        | Geode -> None

type RobotSpec = {
    manufactures: Material
    requires: Map<Material, int>
}

type Robot = {
    spec: RobotSpec;
    creationMinute: int;
}

type BluePrint = {
    planNumber: int
    robotSpecs: Map<Material, RobotSpec>
}

type State = {
    inventory: Map<Material, int>
    robots: Robot list
    minute: int
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
        requires = Map.ofList materials
    }
    
    robotSpec

let makeRobot (bluePrint:BluePrint) (creationMinute: int) (material:Material) =
    let spec = bluePrint.robotSpecs[material]
    let robot = {
        spec = spec
        creationMinute = creationMinute
    }
    robot

// Create states with all possible robot spawning
let rec createAltStates (bluePrint: BluePrint) (rootState:State) (materialOption:Material option) (robotCount:int): State list =
    match materialOption with
        | None -> []
        | Some material ->
            let requirements = bluePrint.robotSpecs[material].requires
            let creationMinute = rootState.minute

            let newInventory =
                
                rootState.inventory 
                    |> Seq.map (fun kvp ->
                                    let requiredAmount =
                                        (Map.tryFind kvp.Key requirements)
                                            |> Option.defaultValue 0
                                            |> (*) robotCount

                                    (kvp.Key, kvp.Value - requiredAmount)
                                )
                                |> Map.ofSeq

            let canCreateRobots = newInventory
                                    |> Map.values
                                    |> Seq.exists (fun v -> v < 0)
                                    |> not

            let newStates =
                if (canCreateRobots)
                then
                    if (robotCount > 0)
                    then
                        let newRobots =
                            seq { 1 .. robotCount}
                                |> Seq.map (fun _ -> makeRobot bluePrint creationMinute material)
                                |> List.ofSeq

                        let newState = { rootState with
                                            robots = rootState.robots |> List.append newRobots
                                            inventory = newInventory
                                       }
                    
                        createAltStates bluePrint newState materialOption (robotCount + 1)
                    else
                        createAltStates bluePrint rootState materialOption (robotCount + 1)
                        
                else
                    createAltStates bluePrint rootState (getSuccessorMaterial material) 0
            
            let uniqueNewStates = newStates |> List.filter (fun s -> s <> rootState)    
            rootState :: uniqueNewStates

let optimizeBlueprint (bluePrint:BluePrint) : State =
    let robot = makeRobot bluePrint 0 Ore
    let robots = [ robot ]

    let initialState = {
            inventory = [(Ore, 0); (Clay, 0); (Obsidian, 0); (Geode, 0)]
                            |> Map.ofSeq
            robots = robots
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

        if (minuteCounter.ContainsKey(state.minute))
        then
            minuteCounter[state.minute] <- minuteCounter[state.minute] + 1
        else
            printfn "Starting minute %d" state.minute
            minuteCounter[state.minute] <- 1

        let robotsByMaterial = state.robots 
                                |> List.groupBy (fun r -> r.spec.manufactures)
                                |> List.map (fun (m, l) -> (m, l.Length))
                                |> Map.ofList

        if (Threshholds |> List.exists (fun (minute, minGeode) ->
                                            state.minute >= minute && state.inventory[Geode] < minGeode))
        then
            ()
        elif (MinRobots |>
                List.exists (fun (t, minRobots) ->
                                    state.minute >= t &&
                                    (minRobots |>
                                        List.exists (fun (m, required) ->
                                                        let has = Map.tryFind m robotsByMaterial
                                                                    |> Option.defaultValue 0
                                                        has < required)
                                    )))
        then
            ()
        else
            let altStates = createAltStates bluePrint state (Some Ore) 0
            //printfn "altStates: %A" altStates
            for state in altStates do
                if (state.minute < MinuteLimit)
                then
                    let minute = state.minute + 1       //Start
                    let newStuff = state.robots
                                    |> List.filter (fun r -> minute > r.creationMinute)
                                    |> List.countBy (fun r -> r.spec.manufactures)
                                    |> Map.ofList
                    let newInventory = state.inventory
                                        |> Map.toList
                                        |> List.map (fun (m, v) -> 
                                                            let toAdd = newStuff 
                                                                            |> Map.tryFind m
                                                                            |> Option.defaultValue 0     
                                                            (m, v + toAdd)
                                                    )
                                        |> Map.ofList
                    let advancedState = { state with
                                            minute = minute
                                            inventory = newInventory
                                        }
                    states.Enqueue(advancedState)
                else
                    finishedStates <- state :: finishedStates
    let best = finishedStates |> List.maxBy (fun s -> s.inventory[Geode])
    best

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
        robotSpecs = [
                        (Ore, oreRobot)
                        (Clay, clayRobot)
                        (Obsidian, obsidianRobot)
                        (Geode, geodeRobot)
                     ] |> Map.ofList
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
