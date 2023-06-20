﻿module Year2022Day19_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Material =
    | Ore
    | Clay
    | Obsidian
    | Geode

type Robot = {
    manufactures: Material
    requires: Map<Material, int>
}

type BluePrint = {
    planNumber: int
    machines: Robot[]
}

let parseMaterial (s:string) : Material =
    match s with
        | "ore" -> Ore
        | "clay" -> Clay
        | "obsidian" -> Obsidian
        | "geode" -> Geode

let makeRobot (m:Match) (robotType:Material) (matchBase:int) : Robot =
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
        
    let robot = {
        manufactures = robotType
        requires = Map.ofList materials
    }
    robot

let parseLine (line:string) : BluePrint =

    let pattern = "Blueprint (\d+): Each ore robot costs (\d+) (\w+)( and (\d+) (\w+))?\. Each clay robot costs (\d+) (\w+)( and (\d+) (\w+))?\. Each obsidian robot costs (\d+) (\w+)( and (\d+) (\w+))?\. Each geode robot costs (\d+) (\w+)( and (\d+) (\w+))?\."

    let m = Regex.Match(line, pattern)
    if (not m.Success)
    then
        failwith "No match"

    let oreRobot = makeRobot m Ore 2
    let clayRobot = makeRobot m Clay 7

    let bluePrint = {
        planNumber = int m.Groups[1].Value
        machines = [| oreRobot; clayRobot |]
    }

    bluePrint
    
let solve =
    let lines = Common.getSampleDataAsArray 2022 19
    // let lines = Common.getChallengeDataAsArray 2022 19
    printAllLines lines
    let plans = lines |> Array.map parseLine
    printfn "%A" plans
    ()
