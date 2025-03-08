module Year2021Day5_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Point = int * int
let getX = fst
let getY = snd

type Line = Point * Point
type Range =
    {
        minValue: int
        maxValue: int
    }

type Extent =
    {
        xRange: Range
        yRange: Range
    }

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

let getEndpoints (segment:Segment) : Point * Point =
    match segment with
        | Horizontal line -> line
        | Vertical line -> line
        | Diagonal (line, _) -> line

let getExtent (segment:Segment) : Extent =
    let ((x1,y1),(x2,y2)) = getEndpoints segment
    let xMin = min x1 x2
    let xMax = max x1 x2
    let yMin = min y1 y2
    let yMax = max y1 y2

    {
        xRange = { minValue = xMin; maxValue = xMax };
        yRange = { minValue = yMin; maxValue = yMax };
    }

let combineExtents (segments:Segment[]) : Extent =
    let extent =
        segments
            |> Array.map getExtent
            |> Array.fold (fun acc extent ->
                            {
                                xRange = { 
                                    minValue = min acc.xRange.minValue extent.xRange.minValue; 
                                    maxValue = max acc.xRange.maxValue extent.xRange.maxValue 
                                };
                                yRange = { 
                                    minValue = min acc.yRange.minValue extent.yRange.minValue; 
                                    maxValue = max acc.yRange.maxValue extent.yRange.maxValue 
                                };
                            }
                          
                          ) {
                                xRange = { minValue = Int32.MaxValue; maxValue = Int32.MinValue };
                                yRange = { minValue = Int32.MaxValue; maxValue = Int32.MinValue };
                            }
    extent

let getPoints (xRange:Range) (yRange:Range) : seq<Point> =
    seq {
        for x in seq { xRange.minValue .. xRange.maxValue } do
            for y in seq { yRange.minValue .. yRange.maxValue } do
                yield (x,y)
    }

let isBetween v v1 v2 : bool =
    let vMin = min v1 v2
    let vMax = max v1 v2
    (v >= vMin) && (v <= vMax)

let segmentContainsPoint (p:Point) (segment:Segment) : bool =
    let (px,py) = p

    match segment with
        | Horizontal ((x1,y),(x2,_)) -> (y = py && (isBetween px x1 x2))
        | Vertical ((x,y1),(_,y2)) -> (x = px && (isBetween py y1 y2))
        | Diagonal (((x1,y1),(x2,y2)), { m = m; b = b; }) ->
            (py = m * px + b) && (isBetween px x1 x2) && (isBetween py y1 y2)

let part1(segments:Segment[]) : unit =

    printfn "There are %d line segments" segments.Length
    //for s in segments do
    //    printfn "%A with extent %A" s (getExtent s)

    let fullExtent = combineExtents segments
    printfn "\nFull extent is %A" fullExtent

    let part1Segments = 
        segments |> 
            Array.choose (fun s ->
                            match s with
                                | Horizontal _ -> Some s
                                | Vertical _ -> Some s
                                | _ -> None
                         )

    let part1Extent = combineExtents part1Segments
    printfn "\nPart 1 extent is %A" fullExtent

    let pointInfo = 
        getPoints part1Extent.xRange part1Extent.yRange
            |> Seq.map (
                    fun point ->
                        let segmentsWithPoint = part1Segments |> Array.filter (segmentContainsPoint point)
                        (point, segmentsWithPoint.Length)
                )
            |> Seq.filter (fun (_, count) -> count >= 2)
            |> List.ofSeq

    for t in pointInfo do
        printfn "Point %A touches %d lines" (fst t) (snd t)

    printfn "There are %d such points." pointInfo.Length

    //printfn "Part 1 segments"
    //for s in part1Segments do
    //    printfn "%A with extent %A" s (getExtent s)

    ()

let part2(segments:Segment[]) : unit =

    printfn "There are %d line segments" segments.Length
    let fullExtent = combineExtents segments
    printfn "\nFull extent is %A" fullExtent

    let pointInfo = 
        getPoints fullExtent.xRange fullExtent.yRange
            |> Seq.map (
                    fun point ->
                        let segmentsWithPoint = segments |> Array.filter (segmentContainsPoint point)
                        (point, segmentsWithPoint.Length)
                )
            |> Seq.filter (fun (_, count) -> count >= 2)
            |> List.ofSeq


    printfn "There are %d matching points." pointInfo.Length

    //printfn "Part 1 segments"
    //for s in part1Segments do
    //    printfn "%A with extent %A" s (getExtent s)

    ()

let solve =
    // let lines = Common.getSampleDataAsArray 2021 5
    let lines = Common.getChallengeDataAsArray 2021 5
    printAllLines lines

    let segments = parseLines lines
    //let result = part1 segments
    let result = part2 segments
    ()