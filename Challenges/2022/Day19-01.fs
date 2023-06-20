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
    robotSpecs: RobotSpec[]
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
    let spec = bluePrint.robotSpecs |> Array.find (fun spec -> spec.manufactures = material)
    let robot = {
        spec = spec
        creationMinute = creationMinute
    }
    robot

let createAltStates (bluePrint: BluePrint, rootState:State) : State list =
    let materials = bluePrint.robotSpecs |> Array.map (fun s -> s.manufactures)

    for material in materials do
        let spec = bluePrint.robotSpecs |> Array.find (fun s -> s.manufactures = material)
        for createCount in seq { 0.. 3} do
            

    []

let optimizeBlueprint (bluePrint:BluePrint) : unit =
    let robot = makeRobot bluePrint 0 Ore
    let robots = [ robot ]

    let state = {
        inventory = Map.empty
        robots = robots
        minute = 0
    }

    let states = Queue<State>()
    states.Enqueue state

    while (states.Count > 0) do
        let state = states.Dequeue()
        let altStates = createAltStates(bluePrint, state)
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
        robotSpecs = [| oreRobot; clayRobot; obsidianRobot; geodeRobot |]
    }

    bluePrint
    
let solve =
    let lines = Common.getSampleDataAsArray 2022 19
    // let lines = Common.getChallengeDataAsArray 2022 19
    printAllLines lines
    let plans = lines |> Array.map parseLine
    printfn "%A" plans
    ()
