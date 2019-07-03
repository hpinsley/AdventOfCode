module Year2018Day06

open System
open System.IO
open Common

type OccupiedCell = int * int

type CellState =
    | Unoccupied
    | Occupied of OccupiedCell

type ClosestCoordinate =
    | None
    | Neighbor of OccupiedCell

let distance (x1, y1) (x2, y2) =
    (abs (x2 - x1)) + (abs (y2 - y1))

let getCoords (inputLine: string) =
    let v = inputLine.Split(",")
                |> Array.map parseInt
    (v.[0], v.[1])

let xCoord (t:int * int): int =
    fst t

let yCoord (t:int * int): int =
    snd t

let getBounds (points: seq<int * int>) =
    let xMax = points |> Seq.map xCoord |> Seq.max
    let yMax = points |> Seq.map yCoord |> Seq.max
    (xMax + 1, yMax + 1)

let setCoordinate (arr: CellState [,]) (x,y) =
    arr.[x, y] <- Occupied <| OccupiedCell (x, y)

// Given the entire coordinate space, find the closest occ
let getClosestCoord (arr:CellState [,]) (x: int) (y: int) coord =
    Unoccupied

let solve =
    // let testdata = Common.getChallengeData 2018 6
    let testdata = Common.getSampleDataAsArray 2018 6

    let points = testdata |> Array.map getCoords

    Array.map (printfn "%O") points |> ignore
    let bounds = getBounds points
    printfn "Bounds: %O" bounds

    let arr =
        Array2D.create (xCoord bounds) (yCoord bounds) Unoccupied

    points |>
        Array.map (setCoordinate arr) |> ignore
    // arr.[0,0] <- 5

    let closestCoord =
        Array2D.mapi (getClosestCoord arr) arr

    printfn "Array is\n%A" arr
    ()
