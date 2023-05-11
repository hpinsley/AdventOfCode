module Year2022Day12_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked

type HeightMap = 
    {
        Heights: char[,];
        StartLocation: int * int;
        EndLocation: int * int;
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

let solve =
    let lines = Common.getSampleDataAsArray 2022 12
    //let lines = Common.getChallengeDataAsArray 2022 12
    for line in lines do
        printfn "%s" line

    printfn ""

    let heightMap = getHeightMap lines
    printGrid heightMap.Heights id

    printfn ""

    printfn "%A" heightMap