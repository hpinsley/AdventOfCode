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
    | Number of int
    | Operation of OpMonkeyDef

type Monkey =
    {
        name: string
        action: MonkeyAction
    }

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

    let m = {
        name = "hi"
        action = Number 1
    }
    
    m

let parseLines (lines:string[]) : Monkey[] =
    lines
        |> Array.map parseLine

let solve =
    let lines = Common.getSampleDataAsArray 2022 21
    // let lines = Common.getChallengeDataAsArray 2022 21
    printAllLines lines

    let parsed = parseLines lines
    printfn "%A" parsed
    ()
