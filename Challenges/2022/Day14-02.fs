module Year2022Day14_Part2

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
type Point = (int * int)
type LineSegment = (Point * Point)

let GetX = fst
let GetY = snd

type GridCell =
    | Empty
    | Rock
    | Sand
    | SandSource
    | Floor

let sandSource = (500, 0)

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

let buildGrid (points:Point[]) : GridCell[,] =
    let minX = points |> Array.map GetX |> Array.min
    let maxX = points |> Array.map GetX |> Array.max
    let minY = points |> Array.map GetY |> Array.min
    let maxY = points |> Array.map GetY |> Array.max
    let floor = maxY + 2

    let grid = Array2D.initBased (minY)  (minX - 2) (maxY - minY + 3) (maxX - minX + 5) (fun _ _ -> Empty)
    points |> Array.iter (fun (x, y) -> match (x, y) with
                                            | (_, _) when (x,y) = sandSource -> 
                                                grid[y,x] <- SandSource
                                            | _ ->
                                                grid[y,x] <- Rock)

    for x = (Array2D.base2 grid) to ((Array2D.base2 grid) + (Array2D.length2 grid) - 1) do
        grid[floor, x] <- Floor

    grid

let displayGrid (grid:GridCell[,]) : unit =
        printGrid grid (fun (cell:GridCell) -> match cell with
                                            | Empty -> '.'
                                            | Rock -> '#'
                                            | Sand -> 'O'
                                            | SandSource -> '+'
                                            | Floor -> 'X')

let dropSandGrain (grid:GridCell[,]) : bool =
    let minY = Array2D.base1 grid
    let yLength = Array2D.length1 grid
    let maxY = minY + yLength - 1

    let mutable noMoreRoom = false
    let mutable fallingIntoAbyss = false

    let mutable (x,y) = sandSource

    while (not (noMoreRoom || fallingIntoAbyss)) do
        let down = (x, y + 1)
        let left = (x - 1, y + 1)
        let right = (x + 1, y + 1)

        let possibles = [down; left; right]     

        let move = possibles |> List.tryFind (fun (x, y) -> grid[y,x] = Empty)
        match move with
            | Some (newX, newY) -> 
                x <- newX
                y <- newY
                if (y >= maxY)
                then
                    fallingIntoAbyss <- true

            | None -> grid[y, x] <- Sand
                      noMoreRoom <- true
    
    not fallingIntoAbyss

let runSimulation (grid:GridCell[,]) : int =
    let mutable grainCount = 0
    while (dropSandGrain grid) do
        grainCount <- grainCount + 1
        //displayGrid grid

    grainCount

let solve =
    let lines = Common.getSampleDataAsArray 2022 14
    //let lines = Common.getChallengeDataAsArray 2022 14
    //printAllLines lines

    //printfn "All points"

    let allPoints = getAllPoints lines |> Array.append [| sandSource |]
    
    //for p in allPoints do
    //    printfn "(%d, %d)" (fst p) (snd p)

    printfn "There are %d points" allPoints.Length

    let grid = buildGrid allPoints
    displayGrid grid

    //printfn "\nSolving...\n"

    //let grainCount = runSimulation grid
    //displayGrid grid
    //printfn "Grain count is %d" grainCount
    ()
