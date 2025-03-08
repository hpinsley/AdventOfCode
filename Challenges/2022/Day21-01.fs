module Year2022Day21_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Opcode =
    | ADD
    | SUB
    | DIV
    | MUL

type OpMonkeyDef =
    {
        opCode: Opcode
        m1: string
        m2: string
    }

type MonkeyAction =
    | Number of int64
    | Operation of OpMonkeyDef

type Monkey =
    {
        name: string
        action: MonkeyAction
    }

let monkeyDict = new Dictionary<string, unit -> int64>()


let buildLookups (monkeys:Monkey[]) : unit =
    let lookupMonkeyResolver name =
        monkeyDict[name]

    let monkeysAndResolvers =
        monkeys
            |> Array.map (fun m -> 
                            match m.action with
                                | Number n -> (m.name, fun () -> n)
                                | Operation opDef ->

                                    let op =
                                        match opDef.opCode with
                                        | Opcode.ADD -> fun () -> 
                                                (lookupMonkeyResolver opDef.m1)() 
                                              + (lookupMonkeyResolver opDef.m2)()
                                        | Opcode.SUB -> fun () -> 
                                                (lookupMonkeyResolver opDef.m1)() 
                                              - (lookupMonkeyResolver opDef.m2)()
                                        | Opcode.MUL -> fun () -> 
                                                (lookupMonkeyResolver opDef.m1)() 
                                              * (lookupMonkeyResolver opDef.m2)()
                                        | Opcode.DIV -> fun () -> 
                                                (lookupMonkeyResolver opDef.m1)() 
                                              / (lookupMonkeyResolver opDef.m2)()
                                    
                                    (m.name, op)
                          )
    for (name, resolver) in monkeysAndResolvers do
        monkeyDict.Add(name, resolver)

let parseLine (line:string) : Monkey =
    let pattern1 = "(....): (....) ([+\-*/]) (....)"
    let pattern2 = "(....): ([0-9]+)"

    let match1 = Regex.Match(line, pattern1)
    let match2 = Regex.Match(line, pattern2)

    if (not (match1.Success || match2.Success))
    then
        failwith "no match"

    if (match1.Success && match2.Success)
    then
        failwith "Both match?"

    if (match1.Success)
    then
        let name = match1.Groups.[1].Value
        let m1 = match1.Groups.[2].Value
        let opCode = match1.Groups.[3].Value
        let m2 = match1.Groups.[4].Value

        let opCode =
            match opCode with
            | "+" -> Opcode.ADD
            | "-" -> Opcode.SUB
            | "*" -> Opcode.MUL
            | "/" -> Opcode.DIV
            | _ -> failwith "unknown opcode"

        let opDef =
            {
                opCode = opCode
                m1 = m1
                m2 = m2
            }

        let action = Operation opDef

        {
            name = name
            action = action
        }
    else
        let name = match2.Groups.[1].Value
        let number = match2.Groups.[2].Value |> int

        let action = Number number

        {
            name = name
            action = action
        }

let parseLines (lines:string[]) : Monkey[] =
    lines
        |> Array.map parseLine

let solve =
    // let lines = Common.getSampleDataAsArray 2022 21
    let lines = Common.getChallengeDataAsArray 2022 21
    //printAllLines lines

    let parsed = parseLines lines
    //printfn "%A" parsed
    buildLookups parsed

    let root = monkeyDict["root"]
    let result = root()
    printfn "%A" result
    ()
