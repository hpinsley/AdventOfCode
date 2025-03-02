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

type State =
    {
        grid: char[,]
        guardStatus: GuardStatus
        visited: Set<Location>
        isDone: bool
    }

let advanceState (state: State): State =
    let newRow = state.guardStatus.location.row + state.guardStatus.direction.deltaY
    let newCol = state.guardStatus.location.col + state.guardStatus.direction.deltaX
    let newLocation = { row = newRow; col = newCol }
    if (newRow < 0 || newRow >= Array2D.length1 state.grid || newCol < 0 || newCol > Array2D.length2 state.grid)
    then
        { state with isDone = true }
    else
        match state.grid[newRow, newCol] with
            | '#' -> { state with guardStatus.direction = turnRight state.guardStatus.direction }
            | _ -> { state with guardStatus.location = { row = newRow; col = newCol}; visited = Set.add newLocation state.visited }

let solve =
    let stopWatch = Stopwatch.StartNew()
    //let lines = Common.getSampleDataAsArray 2024 6
    let lines = Common.getChallengeDataAsArray 2024 6

    let rows = lines.Length
    let cols = lines[0].Length

    let grid = Array2D.init rows cols (fun i j -> lines[i][j])

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
    
    let initialState:State = { grid = grid; guardStatus = guardStatus; visited = Set.ofList [guardStatus.location] ; isDone = false }

    // printfn "Initial state: %A" state

    let mutable state = initialState

    while not state.isDone do
        state <- advanceState state

    let part1Time = stopWatch.ElapsedMilliseconds

    printf "Part 1: The guard visited %d locations" state.visited.Count

    stopWatch.Restart()

    let part2Time = stopWatch.ElapsedMilliseconds;
    printfn "Timings.  Part 1: %dµs, Part 2: %dµs" part1Time part2Time

    ()