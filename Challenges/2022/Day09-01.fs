module Year2022Day09_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions

type Direction =
    | Left | Right | Up | Down

type MoveInstruction =
    | Move of Direction * int

let letterToDirection (letter:char): Direction =
    match letter with
        | 'R' -> Right
        | 'L' -> Left
        | 'U' -> Up
        | 'D' -> Down
        | _ -> sprintf "Unknown letter %A" letter |> failwith

let lineToInstruction (line:string) : MoveInstruction =
    match line with
        | Common.ParseRegex "(.*) (.*)" [direction; steps] -> Move (letterToDirection direction[0], int(steps))
    

let solve =
    // let lines = Common.getSampleDataAsArray 2022 09
    let lines = Common.getChallengeDataAsArray 2022 09

    printfn "%A" lines
    let moves = lines |> Array.map lineToInstruction
    let paired = Array.zip lines moves

    for p in paired do
        printfn "%A => %A" (fst p) (snd p)
    