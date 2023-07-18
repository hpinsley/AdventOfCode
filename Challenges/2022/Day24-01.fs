﻿module Year2022Day24_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Blizzard =
    {
        id: int
        mutable position: (int * int)
        motionDelta: (int * int)
    }

type Cell =
    | Wall
    | Empty
    | Snow of Blizzard

type State =
    {
        grid: Cell[,]
        rows: int
        cols: int
        start: int * int
        finish: int * int
    }

let parseLinesIntoGrid (lines:string[]) : Cell[,] =
    let rowCount = lines.Length
    let colCount = lines[0].Length

    let grid =
        Array2D.init rowCount colCount 
                    (fun row col ->
                        let c = lines[row][col]
                        match c with 
                                | '.' -> Empty
                                | '#' -> Wall
                                | _ ->  Snow {
                                        id = row * colCount + col
                                        position = (row, col)
                                        motionDelta = match c with
                                                        | '^' -> (-1, 0)
                                                        | 'v' -> (1, 0)
                                                        | '<' -> (0, -1)
                                                        | '>' -> (0, 1)
                                    }
                    )
    grid

let findEmptyColumnInRow (grid:Cell[,]) (row:int) : int * int =
    let colCount = Array2D.length2 grid
    let empty = seq { 0 .. (colCount - 1) }
                        |> Seq.map (fun c -> (c, grid[row, c]))
                        |> Seq.find (fun (_, cell) -> match cell with
                                                        | Empty -> true
                                                        | _ -> false
                                    )

    let col = fst empty
    (row, col)

let parseGridIntoState (grid:Cell[,]) : State =
    let rowCount = Array2D.length1 grid
    let colCount = Array2D.length2 grid
    {
        grid = grid
        rows = rowCount
        cols = colCount
        start = findEmptyColumnInRow grid 0
        finish = findEmptyColumnInRow grid (rowCount - 1)
    }

let showTheGrid (grid:Cell[,]) : unit =
    printGrid grid (fun cell -> match cell with
                                    | Empty -> '.'
                                    | Wall -> '#'
                                    | Snow blizzard ->
                                        match blizzard.motionDelta with
                                            | (-1, 0) -> '^'
                                            | (1, 0) -> 'v'
                                            | (0, 1) -> '>'
                                            | (0, -1) -> '<'
                                            | _ -> failwith "Bad motion delta"
                   )

let solveStateSimple (state:State) =
    
    let h (node:int * int) : int =
        manhattan node state.finish
    
    let dist (n1:int * int) (n2:int * int) : int =
        1

    let isGoal (n:int * int) : bool =
        n = state.finish

    let getNeighbors (n:int * int) : (int * int) list =
        let (r,c) = n

        let neighbors = [(-1,0); (1,0); (0,-1); (0,1)]
                            |> List.map (fun (dr,dc) -> (r + dr, c + dc))
                            |> List.filter (fun (r,c) -> 
                                                (r >= 0 && r < state.rows &&
                                                c >= 0 && c < state.cols)
                                           )
        neighbors

    let path = aStar state.start isGoal getNeighbors dist h
    path

let solve =
    let lines = Common.getSampleDataAsArray 2022 24
    // let lines = Common.getChallengeDataAsArray 2022 24
    // printAllLines lines
    let grid = parseLinesIntoGrid lines
    showTheGrid grid
    let state = parseGridIntoState grid
    //printfn "State: %A" state
    printfn "Start at: %A and finish at %A" state.start state.finish

    let path = solveStateSimple state
    printfn "Path:\n%A" path
    ()
