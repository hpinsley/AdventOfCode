module Year2022Day12_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked

type HeightMap = 
    {
        Heights: char[,]
        StartLocation: int * int
        EndLocation: int * int
    }

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
        for i = -1 to 1 do
            for j = -1 to 1 do
                let r = row + i
                let c = col + j

                if (
                        (r >= 0 && r < rowCount)
                    &&  (c >= 0 && c < colCount)
                    &&  ((r <> row) || (c <> col))
                ) then yield (r, c)
    }

let getHeightMap (lines:string[]) : HeightMap =
    let rows = lines.Length
    let cols = lines[0].Length

    let grid = Array2D.init rows cols (fun r c -> lines[r][c])
    let mutable startLocation = (0, 0)
    let mutable endLocation = (0, 0)

    grid |> Array2D.iteri (fun r c character ->
                                match character with
                                    | 'S' -> grid[r,c] <- 'a'
                                             startLocation <- (r,c)
                                    | 'E' -> grid[r,c] <- 'z'
                                             endLocation <- (r,c)
                                    | _ -> ()
                          )
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
    printGrid heightMap.Heights id

    printfn ""

    printfn "%A" heightMap

    let results = runAStar heightMap
    printfn "%A" results

    let rows = Array2D.length1 heightMap.Heights
    let cols = Array2D.length2 heightMap.Heights

    getNeighborLocs rows cols 0 0 |> List.ofSeq |> printfn "TAKE OUT DIAG MOVEMENT: %A"
