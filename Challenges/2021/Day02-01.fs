module Year2021Day2_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Movement =
    | Horizontal of int
    | Vertical of int

type State =
    {
        aim: int
        h: int
        v: int
    }

let parseLineIntoMovement (line:string) : Movement =
    let parts = line.Split(' ')
    match parts[0] with
        | "forward" -> Horizontal (int parts[1])
        | "down" -> Vertical (int parts[1] * 1)
        | "up" -> Vertical (int parts[1] * -1)
        | _ -> failwith "Invalid move operation"

let computePositionPart1 (movements:Movement[]) : (int * int) =
    let state = (0, 0)
    let finalPosition = movements
                            |> Array.fold 
                                (fun (h,v) m ->
                                    match m with
                                        | Horizontal n ->
                                            (h + n, v)
                                        | Vertical n ->
                                            (h, v + n)
                                    
                                )
                                state
    finalPosition

let computePositionPart2 (movements:Movement[]) : (int * int) =
    let state = {
        aim = 0
        h = 0
        v = 0
    }

    let finalState = movements
                            |> Array.fold 
                                (fun s m ->
                                    match m with
                                        | Horizontal n ->
                                            { s with 
                                                    h = s.h + n
                                                    v = s.v + n * s.aim
                                            }
                                        | Vertical n ->
                                            { s with aim = s.aim + n }
                                    
                                )
                                state
    (finalState.h, finalState.v)

let solve =
    // let lines = Common.getSampleDataAsArray 2021 2
    let lines = Common.getChallengeDataAsArray 2021 2
    // printAllLines lines
    let movements = lines |> Array.map parseLineIntoMovement
    //let (h,v) = computePositionPart1 movements
    let (h,v) = computePositionPart2 movements
    let result = h * v
    printfn "Part 2: %d" result
    ()