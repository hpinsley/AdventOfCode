module Year2024Day13_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics
open FSharp.Stats
open FSharp.Stats.Algebra

type T_DIST = double
let parseDistance = Double.Parse

type Button =
    {
        buttonLetter: char
        xDelta: T_DIST
        yDelta: T_DIST
        tokenCost: T_DIST
    }

type Prize =
    {
        xLoc: T_DIST
        yLoc: T_DIST
    }

type Game = 
    {
        AButton: Button
        BButton: Button
        prize: Prize
    }

let parseClawLineToButton (clawLine:string) : Button =
    let pattern = @"Button (.): X\+(\d+), Y\+(\d+)"
    let m = Regex.Match (clawLine, pattern)
    if not m.Success
    then
        raise (Exception("no match"))
    else
        let buttonLetter = m.Groups[1].Value[0]
        let tokenCost = match buttonLetter with
                            |'A' -> 3
                            |'B' -> 1
                            |_ -> raise (Exception("Invalid button letter"))

        {   buttonLetter = buttonLetter; 
            xDelta = parseInt (m.Groups[2].Value) 
            yDelta = parseInt (m.Groups[3].Value)
            tokenCost = tokenCost
        }

let parseClawLinePrize (clawLine:string) : Prize =
    let pattern = @"Prize: X=(.*), Y=(.*)"
    let m = Regex.Match (clawLine, pattern)
    if not m.Success
    then
        raise (Exception("no match"))
    
    {   xLoc = parseInt (m.Groups[1].Value) 
        yLoc = parseInt (m.Groups[2].Value)
    }

let buildGame (AButtonLine: string) (BButtonLine:string) (prizeLine:String) : Game =
    let aButton = parseClawLineToButton AButtonLine
    let bButton = parseClawLineToButton BButtonLine
    let prize = parseClawLinePrize prizeLine

    {
        AButton = aButton; BButton = bButton; prize = prize
    }

let rec parseLinesIntoGames (lines:string[]) : Game list =
    if lines.Length = 0
    then
        []
    elif lines[0] = String.Empty
    then
        parseLinesIntoGames lines[1..]
    else
        let game = buildGame lines[0] lines[1] lines[2]
        game :: parseLinesIntoGames lines[3..]

let isInteger (d:T_DIST) : bool =
    T_DIST.Floor d = d

let playGame (game:Game) : T_DIST option =
    // Assume we use a combination of the buttons

    let v1 = vector [game.AButton.xDelta; game.AButton.yDelta]
    let v2 = vector [game.BButton.xDelta; game.BButton.yDelta]
    let v3 = vector [game.prize.xLoc; game.prize.yLoc]

    let m = matrix [
                    [game.AButton.xDelta; game.BButton.xDelta];
                    [game.AButton.yDelta; game.BButton.yDelta];
                   ]

    let i = LinearAlgebra.Inverse m
    let product = i * v3
    let ATokens = Vector.get product 0
    let BTokens = Vector.get product 1
    let AIsInteger = isInteger ATokens
    let BIsInteger = isInteger BTokens

    let bothButtonCost =
        if AIsInteger && BIsInteger
        then
            Some (ATokens * game.AButton.tokenCost + BTokens * game.BButton.tokenCost)
        else
            None

    bothButtonCost

let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2024 13
    // let lines = Common.getChallengeDataAsArray 2024 13
    
    let games = parseLinesIntoGames lines

    let gameResults = games |> List.map playGame
    //for cost in gameResults do
    //    match cost with
    //        |Some c ->
    //            printfn "Cost is %f" c
    //        |None -> 
    //            printfn "No solution"

    let minCost = gameResults |> List.sumBy (fun opt -> 
                                                match opt with
                                                    | Some c -> c
                                                    | None -> 0
                                            )
    printfn "Part 1: %f" minCost

    let part1Time = stopWatch.ElapsedMilliseconds

    stopWatch.Restart()

    // Part 2
    let part2Time = stopWatch.ElapsedMilliseconds


    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()