module Year2018Day06

open System
open System.IO
open Common

type Coordinate =
    Coordinate of int * int

let getCoords (inputLine: string) =
    let v = inputLine.Split(",")
                |> Array.map parseInt
    (v.[0], v.[1])

let x (t:int * int): int =
    fst t

let y (t:int * int): int =
    snd t

let getBounds (points: seq<int * int>) =
    let xMax = points |> Seq.map x |> Seq.max
    let yMax = points |> Seq.map y |> Seq.max
    (xMax + 1, yMax + 1)

let solve =
    // let testdata = Common.getChallengeData 2018 6
    let testdata = Common.getSampleDataAsArray 2018 6

    let points = testdata |> Array.map getCoords

    Array.map (printfn "%O") points |> ignore
    let bounds = getBounds points
    printfn "Bounds: %O" bounds

    let arr =
        Array2D.init (x bounds) (y bounds) (fun y x -> Coordinate (x,y))

    // arr.[0,0] <- 5

    printfn "Array is\n%A" arr
    ()
