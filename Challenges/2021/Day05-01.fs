module Year2021Day5_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Point = int * int
type Line = Point * Point

type LineFormula = 
    {
        m: int;
        b: int;
    }

type Segment =
    | Horizontal of Line
    | Vertical of Line
    | Diagonal of Line * LineFormula

let parseLine (line:string) : Line =
    let pattern = "(\d+),(\d+) -> (\d+),(\d+)";
    let m = Regex.Match(line, pattern)
    if (m.Success)
    then
        let x1 = m.Groups[1].Value |> int
        let y1 = m.Groups[2].Value |> int
        let x2 = m.Groups[3].Value |> int
        let y2 = m.Groups[4].Value |> int
        ((x1,y1),(x2,y2))
    else
        failwith "Failed to match pattern"

let parseLines (lines:string[]) : Segment[] =
    let endpoints = lines |> Array.map parseLine
    (*
        m = (y2 - y1) / (x2 - x1)

        y = mx + b

        y2 = x2 * (y2 - y1) / (x2 - x1) + b

        b = y2 - x2 * (y2 - y1) / (x2 - x1) 
    *)

    let segments =
        endpoints
            |> Array.map (fun ((x1,y1),(x2,y2)) ->
                            if (x1 = x2)
                            then
                                Vertical ((x1,y1),(x2,y2))
                            elif (y1 = y2)
                            then
                                Horizontal ((x1,y1),(x2,y2))
                            else
                                let m = (y2 - y1) / (x2 - x1)
                                let b = y2 - x2 * m
                                let formala = { m = m; b = b }
                                let l = ((x1,y1),(x2,y2))
                                Diagonal (l, formala)
                        )
    segments

let part1(segments:Segment[]) : unit =
    printfn "There are %d line segments" segments.Length
    for s in segments do
        printfn "%A" s
    ()

let solve =
    let lines = Common.getSampleDataAsArray 2021 5
    // let lines = Common.getChallengeDataAsArray 2021 5
    // printAllLines lines

    let segments = parseLines lines

    let result = part1 segments
    ()