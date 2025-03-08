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

type Status =
    | Patrolling
    | WalkedOffGrid
    | LoopDetected

type State =
    {
        status: Status
        grid: char[,]
        guardStatus: GuardStatus
        visited: Set<Location * MoveDirection>
    }

let advanceState (state: State): State =
    let newRow = state.guardStatus.location.row + state.guardStatus.direction.deltaY
    let newCol = state.guardStatus.location.col + state.guardStatus.direction.deltaX
    let newLocation = { row = newRow; col = newCol }
    if (newRow < 0 || newRow >= Array2D.length1 state.grid || newCol < 0 || newCol >= Array2D.length2 state.grid)
    then
        { state with status = WalkedOffGrid }
    else
        match state.grid[newRow, newCol] with
            | '#' -> { state with guardStatus.direction = turnRight state.guardStatus.direction }
            | _ when Set.contains (newLocation,state.guardStatus.direction) state.visited -> { state with status = LoopDetected }
            | _ -> { state with guardStatus.location = { row = newRow; col = newCol}; visited = Set.add (newLocation, state.guardStatus.direction) state.visited }

let solve =
    let stopWatch = Stopwatch.StartNew()
    // let lines = Common.getSampleDataAsArray 2024 6
    let lines = Common.getChallengeDataAsArray 2024 6

    let rows = lines.Length
    let cols = lines[0].Length

    let grid = Array2D.init rows cols (fun i j -> lines[i][j])

    // Find the guard

    let mutable guardStatus:GuardStatus = { location = { row = -1; col = -1}; direction = up }

    // Find the guard
    grid |> Array2D.iteri ( fun r c elem -> 
                              match elem with
                                | '^' -> guardStatus <- { location = { row = r; col = c}; direction = up }
                                | '>' -> guardStatus <- { location = { row = r; col = c}; direction = right }
                                | 'v' -> guardStatus <- { location = { row = r; col = c}; direction = down }
                                | '<' -> guardStatus <- { location = { row = r; col = c}; direction = left }
                                | _ -> ()
                          )
    
    // printGrid grid id
    // printfn "Initial guard status is %A" guardStatus


    let initialState:State = { grid = grid; guardStatus = guardStatus; visited = Set.ofList [(guardStatus.location, guardStatus.direction)] ; status = Patrolling }

    // printfn "Initial state: %A" state

    let mutable state = initialState

    while state.status = Patrolling do
        //printfn "DEBUG: %A The guard visited %d locations.  Guard State: [%d,%d](%d,%d)" state.status state.visited.Count
        //                state.guardStatus.location.row
        //                state.guardStatus.location.col
        //                state.guardStatus.direction.deltaX
        //                state.guardStatus.direction.deltaY

        state <- advanceState state

    let part1Time = stopWatch.ElapsedMilliseconds

    // The visited list included directions.  We want the distinct let of locations

    let visited = Set.map (fun (loc, _) -> loc) state.visited
    let visitCount = visited.Count

    printfn "Part 1:  %A The guard visited %d locations.  Guard State: [%d,%d](%d,%d)" state.status visitCount
                    state.guardStatus.location.row
                    state.guardStatus.location.col
                    state.guardStatus.direction.deltaX
                    state.guardStatus.direction.deltaY

    stopWatch.Restart()

    // Part 2

    let obstacleLocations = new HashSet<Location>()

    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid

    for r in seq {0..(rows-1) } do
        for c in seq {0..(cols-1) } do
            let location = { row = r; col = c }
            // printfn "Testing %d, %d" r c
            let mutable symbol = grid[r,c]
            if symbol = '.'
            then
                Array2D.set grid r c '#'    // Set an obstacle here and see if this causes a loop
                state <- { grid = grid; guardStatus = guardStatus; visited = Set.ofList [(guardStatus.location, guardStatus.direction)] ; status = Patrolling }
                while state.status = Patrolling do
                    state <- advanceState state
                if state.status = LoopDetected
                then
                    obstacleLocations.Add(location) |> ignore

                Array2D.set grid r c symbol
            else
                ()

    printfn "Part 2: Obstacles can be placed in %d locations." obstacleLocations.Count

    let part2Time = stopWatch.ElapsedMilliseconds;
    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()