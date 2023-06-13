module Year2022Day18_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Point = {
    x: int
    y: int
    z: int
}

type SideName =
    | Bottom
    | Top
    | Left
    | Right
    | Front
    | Back

type Side = Point[]
type NamedSide =
    {
        sideName: SideName
        points: Set<Point>
    }

type Cube = {
    cubeNumber: int;
    bottom: NamedSide
    top: NamedSide
    left: NamedSide
    right: NamedSide
    front: NamedSide
    back: NamedSide
}

let generateCube (cubeNumber:int) (origin:Point): Cube =
    let bottom  = [|    { x = origin.x; y = origin.y; z = origin.z }; 
                    { x = origin.x; y = origin.y + 1; z = origin.z };
                    { x = origin.x + 1; y = origin.y; z = origin.z };
                    { x = origin.x + 1; y = origin.y + 1; z = origin.z }
                    |] |> Set.ofArray

    let top  = [|       { x = origin.x; y = origin.y; z = origin.z + 1}; 
                    { x = origin.x; y = origin.y + 1; z = origin.z + 1 };
                    { x = origin.x + 1; y = origin.y; z = origin.z + 1};
                    { x = origin.x + 1; y = origin.y + 1; z = origin.z + 1 }
                    |] |> Set.ofArray

    let left  = [|      { x = origin.x; y = origin.y; z = origin.z}; 
                    { x = origin.x; y = origin.y + 1; z = origin.z };
                    { x = origin.x; y = origin.y; z = origin.z + 1};
                    { x = origin.x; y = origin.y + 1; z = origin.z + 1 }
                    |] |> Set.ofArray
    let right  = [|      { x = origin.x + 1; y = origin.y; z = origin.z}; 
                    { x = origin.x + 1; y = origin.y + 1; z = origin.z };
                    { x = origin.x + 1; y = origin.y; z = origin.z + 1};
                    { x = origin.x + 1; y = origin.y + 1; z = origin.z + 1 }
                    |] |> Set.ofArray

    let back  = [|      { x = origin.x; y = origin.y + 1; z = origin.z}; 
                    { x = origin.x + 1; y = origin.y + 1; z = origin.z };
                    { x = origin.x; y = origin.y + 1; z = origin.z };
                    { x = origin.x + 1; y = origin.y + 1; z = origin.z + 1 }
                    |] |> Set.ofArray
    let front  = [|     { x = origin.x; y = origin.y; z = origin.z}; 
                    { x = origin.x + 1; y = origin.y; z = origin.z };
                    { x = origin.x; y = origin.y; z = origin.z };
                    { x = origin.x + 1; y = origin.y; z = origin.z + 1 }
                    |] |> Set.ofArray

    let (cube:Cube) = {
                        cubeNumber = cubeNumber
                        bottom = { sideName = Bottom; points = bottom }
                        top = { sideName = Top; points = top }
                        left = { sideName = Left; points = left }
                        right = { sideName = Right; points = right }
                        front = { sideName = Front; points = front }
                        back = { sideName = Back; points = back }
                      }

    cube

let getCommonSide (cube1:Cube) (cube2:Cube): (NamedSide * NamedSide) option =
    let comparisons = seq {
        (cube1.left, cube2.right); (cube1.right, cube2.left)
        (cube1.top, cube2.bottom); (cube1.bottom, cube2.top)
        (cube1.front, cube2.back); (cube1.back, cube2.front)
    }
    
    comparisons |> Seq.tryFind (fun (side1, side2) -> side1.points = side2.points)
    

let getOriginPoints (lines:string[]) : Point[] =
    lines |>
        Array.map (fun s -> 
                    let split = s.Split(',')
                    {
                        x = int split[0]
                        y = int split[1]
                        z = int split[2]
                    }
                )

let solve =
    // let lines = Common.getSampleDataAsArray 2022 18
    // let lines = Common.getChallengeDataAsArray 2022 18
    let lines = [| "1,1,1"; "2,1,1"|]
    // printAllLines lines
    let originPoints = getOriginPoints lines
    printfn "There are %d cubes to build" originPoints.Length
    printfn ""

    let cubes = originPoints |> Array.mapi generateCube

    match getCommonSide cubes[0] cubes[1] with
                | Some (side1, side2) ->
                        printfn "Match: Cube1: %A with Cube2: %A" side1.sideName side2.sideName
                | None ->
                        printfn "No match"


    ()
