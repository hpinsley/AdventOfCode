module Year2022Day22_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type TurnDirection =
    | Clockwise
    | CounterClockwise

type Action =
    | Move of int
    | Turn of TurnDirection

let parseActions (line:string) : Action[] =
    let pattern = "((\d+)|(L|R)*)+"
    let m = Regex.Match(line, pattern)
    if m.Success then
        let g = m.Groups.[1]
        let actions = g.Captures
                            |> Seq.filter (fun c -> c.Value.Length > 0)
                            |> Seq.map (fun c ->
                                            match c.Value with
                                                | "L" -> Turn CounterClockwise
                                                | "R" -> Turn Clockwise
                                                | _ -> Move (int c.Value)
                            )
                    |> Seq.toArray
        actions
    else
        failwith "No match"

let parseIntoModel (lines:string[]) : unit =
    let l = lines.Length
    let top = lines[0..(l - 3)]
    let bottom = lines[l - 1]
    let actions = parseActions bottom
    printfn "Actions:\n%A" actions
    ()

let solve =
    let lines = Common.getSampleDataAsArray 2022 22
    // let lines = Common.getChallengeDataAsArray 2022 22
    //printAllLines lines
    parseIntoModel lines
    ()
