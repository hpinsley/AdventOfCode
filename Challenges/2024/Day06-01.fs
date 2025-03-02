module Year2024Day6_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

let Obstacle = '#'

type Location =
    {
        row: int
        col: int
    }


type MoveDirection = 
    {
        deltaX: int
        deltaY: int
    }

let up = { deltaX = 0; deltaY = -1 }
let down = { deltaX = 0; deltaY = 1 }
let right = { deltaX = 1; deltaY = 0 }
let left = { deltaX = -1; deltaY = 0 }

type GuardStatus =
    {
        location: Location
        direction: MoveDirection
    }

let turnRight (moveDirection: MoveDirection) =
    match moveDirection with
                        | _ when moveDirection = up -> right 
                        | _ when moveDirection = down -> left
                        | _ when moveDirection = right -> down
                        | _ when moveDirection = left  -> up
                        | _ -> raise (Exception("Unknown direction"))

let solve =
    let stopWatch = Stopwatch.StartNew()
    let lines = Common.getSampleDataAsArray 2024 6
    // let lines = Common.getChallengeDataAsArray 2024 6

    let rows = lines.Length
    let cols = lines[0].Length

    let grid = Array2D.init rows cols (fun i j -> lines[i][j])
    printGrid grid id

    // Find the guard

    let mutable guardStatus:GuardStatus = { location = { row = -1; col = -1}; direction = up }

    grid |> Array2D.iteri ( fun r c elem -> 
                              match elem with
                                | '^' -> guardStatus <- { location = { row = r; col = c}; direction = up }
                                | '>' -> guardStatus <- { location = { row = r; col = c}; direction = right }
                                | 'v' -> guardStatus <- { location = { row = r; col = c}; direction = down }
                                | '<' -> guardStatus <- { location = { row = r; col = c}; direction = left }
                                | _ -> ()
                          )
    
    printfn "The guard is located at %A" guardStatus

    let part1Time = stopWatch.ElapsedMilliseconds

    stopWatch.Restart()

    let part2Time = stopWatch.ElapsedMilliseconds;
    printfn "Timings.  Part 1: %dµs, Part 2: %dµs" part1Time part2Time

    ()