module Year2022Day22_Part1

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

type Row = int
type Col = int

type State =
    {
        grid: GridCellType[,]
        rowBoundaries: (int * int)[]
        colBoundaries: (int * int)[]
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

let parseIntoModel (lines:string[]) : unit =
    let l = lines.Length
    let top = lines[0..(l - 3)]
    let bottom = lines[l - 1]

    printfn "Parsed"
    let grid = parseGrid top
    //displayGrid grid

    let actions = parseActions bottom
    printfn "Actions:\n%A" actions

    // Get the boundaries of each row and column
    let rowBoundaries =
        seq { 0 .. (Array2D.base1 grid) - 1}
            |> Seq.map (fun row ->
                            
                            )
        

    ()

let solve =
    let lines = Common.getSampleDataAsArray 2022 22
    // let lines = Common.getChallengeDataAsArray 2022 22
    printAllLines lines
    parseIntoModel lines
    ()
