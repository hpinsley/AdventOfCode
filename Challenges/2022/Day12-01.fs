module Year2022Day12_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type HeightMap = 
    {
        Heights: int[,]
        StartLocation: int * int
        EndLocation: int * int
    }

let getRow = fst
let getCol = snd

type Neighbor =
    {
        Loc: int * int

    }
type Node =
    {
        Loc: int * int
        Parent: Option<int * int>
    }

let getNeighborLocs rowCount colCount row col =
    seq {
        if (row > 0) then 
            yield (row - 1, col)
        if (row < rowCount - 1) then
            yield (row + 1, col)
        if (col > 0) then 
            yield (row, col - 1)
        if (col < colCount - 1) then
            yield (row, col + 1)
    }

let getHeightMap (lines:string[]) : HeightMap =
    let rows = lines.Length
    let cols = lines[0].Length

    let grid = Array2D.create rows cols 0
    let mutable startLocation = (0, 0)
    let mutable endLocation = (0, 0)

    for r = 0 to rows - 1 do
        for c = 0 to cols - 1 do
            let h = lines[r][c]
            match h with
                | 'S' -> grid[r,c] <- int 'a' - int 'a'
                         startLocation <- (r,c)
                | 'E' -> grid[r,c] <- int 'z' - int 'a'
                         endLocation <- (r,c)
                | _ -> grid[r,c] <- int h - int 'a'

    let heights = {
        Heights = grid
        StartLocation = startLocation
        EndLocation = endLocation
    }
    
    heights

let runAStar (heightMap:HeightMap) : int =
    0

let solve =
    let lines = Common.getSampleDataAsArray 2022 12
    // let lines = Common.getChallengeDataAsArray 2022 12
    for line in lines do
        printfn "%s" line

    printfn ""

    let heightMap = getHeightMap lines
    printGrid heightMap.Heights char

    printfn ""

    printfn "%A" heightMap

    let results = runAStar heightMap
    printfn "%A" results

    let rows = Array2D.length1 heightMap.Heights
    let cols = Array2D.length2 heightMap.Heights

    let gScore:Map<(int * int), int> = Map.empty |> Map.add heightMap.StartLocation 0
    let openSet: PriorityQueue<(int * int), int> = PriorityQueue()

    
    let testCell = (1, 2)
    getNeighborLocs rows cols (getRow testCell) (getCol testCell) |> List.ofSeq |> printfn "Neighbors of %A: %A" testCell
