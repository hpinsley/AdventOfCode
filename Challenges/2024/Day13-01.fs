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

type Button =
    {
        buttonLetter: char
        xDelta: int
        yDelta: int
        tokenCost: int
    }

type Prize =
    {
        xLoc: int
        yLoc: int
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
        
let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2024 13
    // let lines = Common.getChallengeDataAsArray 2024 13
    
    let games = parseLinesIntoGames lines

    let part1Time = stopWatch.ElapsedMilliseconds

    stopWatch.Restart()

    // Part 2
    let part2Time = stopWatch.ElapsedMilliseconds


    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()