module Year2022Day14_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
type Point = (int * int)
type LineSegment = (Point * Point)


let parsePoint (p:string) : Point =
    let m = Regex("(\d+),(\d+)").Match(p)
    if (not m.Success)
    then
        failwith "Bad match"

    (int m.Groups[1].Value, int m.Groups[2].Value)

let parseIntoLineSegments (line:string) : LineSegment[] =
    let points = line.Split(" -> ")
                |> Array.map parsePoint
    let lineSegments = points  |> Array.pairwise
    lineSegments
 
let lineSegmentToPoints (segment:LineSegment) : Point[] =
    let p1 = fst segment
    let p2 = snd segment
    let x1 = fst p1
    let x2 = fst p2
    let y1 = snd p1
    let y2 = snd p2
    let xInc = if (x2 >= x1) then 1 else -1
    let yInc = if (y2 >= y1) then 1 else -1
    let xSeq = seq { x1 .. xInc .. x2 } |> List.ofSeq
    let ySeq = seq { y1 .. yInc .. y2 } |> List.ofSeq
    let points = seq { for x in xSeq do for y in ySeq do yield (x, y) } |> Array.ofSeq
    points

let parseRow (line:string) : Point[][] =
    let lineSegments = parseIntoLineSegments line
    lineSegments |> Array.map lineSegmentToPoints

let getAllPoints (lines:string[]) : Point[] =
    let allPoints = lines |> Array.map parseRow |> Array.concat |> Array.concat |> Array.distinct
    allPoints

let solve =
    let lines = Common.getSampleDataAsArray 2022 14
    //let lines = Common.getChallengeDataAsArray 2022 14
    printAllLines lines

    printfn "All points"

    let allPoints = getAllPoints lines
    
    for p in allPoints do
        printfn "(%d, %d)" (fst p) (snd p)

    printfn "There are %d points" allPoints.Length

