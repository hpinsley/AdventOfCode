module Year2022Day19_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let MinuteLimit = 24

type Material =
    | Ore
    | Clay
    | Obsidian
    | Geode

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
            
            rootState :: newStates    
    
let optimizeBlueprint (bluePrint:BluePrint) : unit =
    let robot = makeRobot bluePrint 0 Ore
    let robots = [ robot ]

    let state = {
        inventory = [(Ore, 0); (Clay, 0); (Obsidian, 0); (Geode, 0)]
                        |> Map.ofSeq
        robots = robots
        minute = 0
    }

    let states = Queue<State>()
    states.Enqueue state

    while (states.Count > 0) do
        let state = states.Dequeue()
        let altStates = createAltStates bluePrint state (Some Ore) 0
        printfn "altStates: %A" altStates
        ()
    ()

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
