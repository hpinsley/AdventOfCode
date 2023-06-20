module Year2022Day19_Part1

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

type Machine = {
    manufactures: Material
    requires: Map<Material, int>
}

type BluePrint = {
    planNumber: int
    machines: Machine[]
}

let parseLine (line:string) : BluePrint =

    let pattern = "Blueprint (\d+): Each ore robot costs (\d+) (\w+)(<oreExtra> and (\d+) (\w+))?\. Each clay robot costs (\d+) (\w+)(<clayExtra> and (\d+) (\w+))?\. Each obsidian robot costs (\d+) (\w+)(<obsidianExtra> and (\d+) (\w+))?\. Each geode robot costs (\d+) (\w+)( and (\d+) (\w+))?\."
    let pattern = "Blueprint (\d+): Each ore robot costs (\d+) (\w+)(<oreExtra> and (\d+) (\w+))?\. Each clay robot costs (\d+) (\w+)\. Each obsidian robot costs 3 ore and 14 clay\. Each geode robot costs 2 ore and 7 obsidian\."
    let pattern = "Blueprint 1: Each ore robot costs 4 ore\. Each clay robot costs 2 ore\. Each obsidian robot costs 3 ore and 14 clay\. Each geode robot costs 2 ore and 7 obsidian\."
    let pattern = "Blueprint (\d+): Each ore robot costs (\d+) (\w+)(<oreExtra> and (\d+) (\w+))?\. Each clay robot costs 2 ore\. Each obsidian robot costs 3 ore and 14 clay\. Each geode robot costs 2 ore and 7 obsidian\."
    let pattern = "Blueprint (\d+): Each ore robot costs (\d+) (\w+)(<oreExtra> and (\d+) (\w+))?\. Each clay robot costs (\d+) (\w+)\. Each obsidian robot costs 3 ore and 14 clay\. Each geode robot costs 2 ore and 7 obsidian\."
    let pattern = "Blueprint (\d+): Each ore robot costs (\d+) (\w+)(<oreExtra> and (\d+) (\w+))?\. Each clay robot costs (\d+) (\w+)\. Each obsidian robot costs (\d+) (\w+) and 14 clay\. Each geode robot costs 2 ore and 7 obsidian\."
    let pattern = "Blueprint (\d+): Each ore robot costs (\d+) (\w+)(<oreExtra> and (\d+) (\w+))?\. Each clay robot costs (\d+) (\w+)\. Each obsidian robot costs (\d+) (\w+)( and 14 clay)?\. Each geode robot costs 2 ore and 7 obsidian\."
    let pattern = "Blueprint (\d+): Each ore robot costs (\d+) (\w+)(<oreExtra> and (\d+) (\w+))?\. Each clay robot costs (\d+) (\w+)\. Each obsidian robot costs (\d+) (\w+)( and (\d+) clay)?\. Each geode robot costs 2 ore and 7 obsidian\."
    let pattern = "Blueprint (\d+): Each ore robot costs (\d+) (\w+)(<oreExtra> and (\d+) (\w+))?\. Each clay robot costs (\d+) (\w+)\. Each obsidian robot costs (\d+) (\w+)( and (\d+) (\w+))?\. Each geode robot costs 2 ore and 7 obsidian\."
    let pattern = "Blueprint (\d+): Each ore robot costs (\d+) (\w+)(<oreExtra> and (\d+) (\w+))?\. Each clay robot costs (\d+) (\w+)(<clayExtra> and (\d+) (\w+))?\. Each obsidian robot costs (\d+) (\w+)( and (\d+) (\w+))?\. Each geode robot costs 2 ore and 7 obsidian\."
    let pattern = "Blueprint (\d+): Each ore robot costs (\d+) (\w+)(<oreExtra> and (\d+) (\w+))?\. Each clay robot costs (\d+) (\w+)(<clayExtra> and (\d+) (\w+))?\. Each obsidian robot costs (\d+) (\w+)( and (\d+) (\w+))?\. Each geode robot costs 2 ore and 7 obsidian\."
    let pattern = "Blueprint (\d+): Each ore robot costs (\d+) (\w+)(<oreExtra> and (\d+) (\w+))?\. Each clay robot costs (\d+) (\w+)(<clayExtra> and (\d+) (\w+))?\. Each obsidian robot costs (\d+) (\w+)( and (\d+) (\w+))?\. Each geode robot costs 2 ore and 7 obsidian\."
    let pattern = "Blueprint (\d+): Each ore robot costs (\d+) (\w+)(<oreExtra> and (\d+) (\w+))?\. Each clay robot costs (\d+) (\w+)(<clayExtra> and (\d+) (\w+))?\. Each obsidian robot costs (\d+) (\w+)( and (\d+) (\w+))?\. Each geode robot costs (\d+) (\w+)( and (\d+) (\w+))?\."
    let pattern = "Blueprint (\d+): Each ore robot costs (\d+) (\w+)( and (\d+) (\w+))?\. Each clay robot costs (\d+) (\w+)( and (\d+) (\w+))?\. Each obsidian robot costs (\d+) (\w+)( and (\d+) (\w+))?\. Each geode robot costs (\d+) (\w+)( and (\d+) (\w+))?\."

    let m = Regex.Match(line, pattern)
    if (not m.Success)
    then
        failwith "No match"

    let bluePrint = {
        planNumber = 1
        machines = [||]
    }

    bluePrint
    
let solve =
    let lines = Common.getSampleDataAsArray 2022 19
    //let lines = Common.getChallengeDataAsArray 2022 19
    printAllLines lines
    //let machines = lines |> Array.map parseLine
    //printfn "%A" machines
    parseLine lines[0] |> ignore
    ()
