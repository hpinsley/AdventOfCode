module Year2021Day5_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Point = int * int
type Line = Point * Point

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

let parseLines (lines:string[]) : Line[] =
    lines |> Array.map parseLine

let part1(segments:Line[]) : unit =
    printfn "There are %d line segments" segments.Length

let solve =
    let lines = Common.getSampleDataAsArray 2021 5
    // let lines = Common.getChallengeDataAsArray 2021 5
    // printAllLines lines

    let segments = parseLines lines
    let result = part1 segments

    ()