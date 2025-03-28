﻿module Year2022Day02_Part2

open System
open System.IO
open Common
open System.Text.RegularExpressions

type Shape =
    | Rock
    | Paper
    | Scissors

let letterToShape (c:char): Shape =
    match c with
        | 'A' -> Rock
        | 'B' -> Paper
        | 'C' -> Scissors

let shapeValue (s:Shape): int =
    match s with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

type Outcome = Win | Lose | Draw

let letterToOutcome (c:char): Outcome =
    match c with
        | 'X' -> Lose
        | 'Y' -> Draw
        | 'Z' -> Win

let playForOutcome (hisShape:Shape) (desiredOutcome:Outcome) : Shape =
    match desiredOutcome with
        | Draw -> hisShape
        | Win ->
            match hisShape with
                | Rock -> Paper
                | Paper -> Scissors
                | Scissors -> Rock
        | Lose ->
            match hisShape with
                | Rock -> Scissors
                | Paper -> Rock
                | Scissors -> Paper


let outcomeValue o =
    match o with
        | Win -> 6
        | Lose -> 0
        | Draw -> 3

let playRound (myShape:Shape) (hisShape:Shape): Outcome * Shape =
    let outcome = if (myShape = hisShape) 
                    then Draw
                    else
                        match myShape with
                            | Rock -> match hisShape with
                                            | Paper -> Lose
                                            | Scissors -> Win
                            | Paper -> match hisShape with
                                            | Rock -> Win
                                            | Scissors -> Lose
                            | Scissors -> match hisShape with
                                            | Rock -> Lose
                                            | Paper -> Win
    (outcome, myShape)


let solve =

    // let lines = Common.getSampleDataAsArray 2022 02
    let lines = Common.getChallengeDataAsArray 2022 02
    printfn "%A" lines
    let pairs = lines
                    |> Seq.map (fun s -> s.Trim().Split())
                    |> Seq.map (fun v2 -> (v2.[0].[0], v2.[1].[0]))
                    |> Seq.map (fun (c1, c2) -> (letterToShape c1, letterToOutcome c2))
                    |> Seq.map (fun (hisShape, outcome) -> (hisShape, playForOutcome hisShape outcome))

    printfn "%A" pairs 

    let outcomes = pairs |> Seq.map (fun t -> playRound (snd t) (fst t))
    printfn "%A" outcomes

    let roundValues = outcomes |> Seq.map (fun (outcome, shape) -> outcomeValue outcome + shapeValue shape)
    printfn "%A" (roundValues |> Seq.sum)
