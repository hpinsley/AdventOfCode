﻿module Year2022Day22_Part2

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type TurnDirection =
    | Clockwise
    | CounterClockwise

type Action =
    | Move of int
    | Turn of TurnDirection

type GridCellType =
    | Tile
    | Wall
    | OutOfBounds

type Facing =
    | Left
    | Right
    | Up
    | Down

type Side =
    | Top
    | Bottom
    | Left
    | Right
    | Front
    | Back

type Row = int
type Col = int


let rules =
    [|
        ( Side.Top, Facing.Left, Side.Left, Facing.Down )
        ( Side.Top, Facing.Right, Side.Right, Facing.Down)
        ( Side.Top, Facing.Up, Side.Back, Facing.Down)
        ( Side.Top, Facing.Down, Side.Front, Facing.Down)

        ( Side.Top, Facing.Left, Side.Left, Facing.Down )
        ( Side.Top, Facing.Right, Side.Right, Facing.Down)
        ( Side.Top, Facing.Up, Side.Back, Facing.Down)
        ( Side.Top, Facing.Down, Side.Front, Facing.Down)
    |]

type State =
    {
        grid: GridCellType[,]
        remainingActions: Action list
        currentFacing: Facing
        currentCell: Row * Col
    }

let parseActions (line:string) : Action[] =
    let pattern = "((\d+)|(L|R)*)+"
    let m = Regex.Match(line, pattern)
    if m.Success then
        let g = m.Groups.[1]
        let actions = g.Captures
                            |> Seq.filter (fun c -> c.Value.Length > 0)
                            |> Seq.map (fun c ->
                                            match c.Value with
                                                | "L" -> Turn CounterClockwise
                                                | "R" -> Turn Clockwise
                                                | _ -> Move (int c.Value)
                            )
                    |> Seq.toArray
        actions
    else
        failwith "No match"

let parseGrid (lines:string[]) : GridCellType[,] =
    let rowCount = lines.Length
    let maxLineLength = lines 
                            |> Array.maxBy (fun line -> line.Length)
                            |> String.length

    let grid = Array2D.init 
                rowCount maxLineLength
                (fun row col ->
                    let line = lines[row]
                    if (col >= line.Length)
                    then
                        OutOfBounds
                    else
                        match line[col] with
                            | ' ' -> OutOfBounds
                            | '.' -> Tile
                            | '#' -> Wall
                            | _ -> failwith "Unknown char"
                )
    grid

let displayGrid (grid:GridCellType[,]) : unit =
    printGrid grid (fun c -> match c with
                        | OutOfBounds -> ' '
                        | Tile -> '.'
                        | Wall -> '#'
                    )


let moveStateTurn (state:State) (direction:TurnDirection) : State =
    let newFacing =
        match state.currentFacing with
            | Facing.Left ->
                match direction with
                    | Clockwise -> Up
                    | CounterClockwise -> Down
            | Facing.Right ->
                match direction with
                    | Clockwise -> Down
                    | CounterClockwise -> Up
            | Up ->
                match direction with
                    | Clockwise -> Facing.Right
                    | CounterClockwise -> Facing.Left
            | Down ->
                match direction with
                    | Clockwise -> Facing.Left
                    | CounterClockwise -> Facing.Right

    { state with currentFacing = newFacing }


let parseIntoModel (lines:string[]) (sideLength:int) (sectorMap:Side option list): unit =
    let l = lines.Length
    let top = lines[0..(l - 3)]
    let bottom = lines[l - 1]

    printfn "Parsed"
    let grid = parseGrid top
    //displayGrid grid

    let actions = parseActions bottom
    //printfn "Actions:\n%A" actions

    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid

    printfn "Grid is %d rows x %d cols" rows cols

    let rowSectors = rows / sideLength
    let colSectors = cols / sideLength

    printfn "Given the side length of %d, that means %d row sectors and %d col sectors" sideLength rowSectors colSectors

    let sectorCount = rowSectors * colSectors
    printfn "There are %d sectors" sectorCount

    if (sectorMap.Length <> sectorCount)
    then
        failwith "Sector map does not match sector count"

    let startingRow = 0
    let startingCol =
        seq { 0 .. Array2D.length2 grid}
            |> Seq.tryFind (fun col -> grid.[startingRow, col] = Tile)
            |> Option.defaultValue 0

    let state = {
        grid = grid
        remainingActions = List.ofSeq actions
        currentFacing = Facing.Right
        currentCell = (startingRow, startingCol)
    }

    //let finalState = moveState state

    //let row = 1 + fst finalState.currentCell
    //let col = 1 + snd finalState.currentCell
    //let facing = match finalState.currentFacing with
    //                | Facing.Right -> 0
    //                | Down -> 1
    //                | Facing.Left -> 2
    //                | Up -> 3

    //let score = 1000 * row + 4 * col + facing

    let score = 0
    printfn "Final score: %d" score
    ()

let solve =
    let lines = Common.getSampleDataAsArray 2022 22
    // These have to change depending on which data set you are solving
    let sideLength = 4
    let sectorMap = [
        None; None; Some Side.Top; None;
        Some Side.Back; Some Side.Left ; None; None;
        None; None; Some Side.Bottom; Some Side.Right
    ]
    
    let lines = Common.getChallengeDataAsArray 2022 22
    // These have to change depending on which data set you are solving
    let sideLength = 50
    let sectorMap = [
        None; Some Side.Back; Some Side.Right; 
        None; Some Side.Top; None; 
        Some Side.Left; Some Side.Front; None; 
        Some Side.Bottom; None; None 
    ]

    //printAllLines lines
    parseIntoModel lines sideLength sectorMap
    ()
