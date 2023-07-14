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

let getRowBoundariesOfGrid (grid:GridCellType[,]) : (int * int)[] =
    // Get the boundaries of each row and column
    let rowBoundaries =
        let rowLimit = (Array2D.length1 grid) - 1
        let colLimit = (Array2D.length2 grid) - 1

        seq { 0 .. rowLimit }
            |> Seq.map (fun row ->
                    
                            let cols = seq { 0 .. colLimit }
                                        |> Seq.map (fun col -> (col, grid.[row, col]))
                                        |> List.ofSeq
                            
                            let firstCol = 
                                cols 
                                    |> List.tryFind (fun (_, c) -> c <> OutOfBounds)
                                    |> Option.defaultValue cols[0]

                            let lastCol =
                                cols
                                    |> List.rev
                                    |> List.tryFind (fun (_, c) -> c <> OutOfBounds)
                                    |> Option.defaultValue cols.[colLimit]
                            
                            (fst firstCol, fst lastCol)
                        )
            |> Array.ofSeq        
    
    rowBoundaries

let getColBoundariesOfGrid (grid:GridCellType[,]) : (int * int)[] =
    // Get the boundaries of each row and column
    let colBoundaries =
        let rowLimit = (Array2D.length1 grid) - 1
        let colLimit = (Array2D.length2 grid) - 1

        seq { 0 .. colLimit }
            |> Seq.map (fun col ->
                    
                            let rows = seq { 0 .. rowLimit }
                                        |> Seq.map (fun row -> (row, grid.[row, col]))
                                        |> List.ofSeq
                            
                            let firstRow = 
                                rows
                                    |> List.tryFind (fun (_, c) -> c <> OutOfBounds)
                                    |> Option.defaultValue rows[0]

                            let lastRow =
                                rows
                                    |> List.rev
                                    |> List.tryFind (fun (_, c) -> c <> OutOfBounds)
                                    |> Option.defaultValue rows.[rowLimit]
                            
                            (fst firstRow, fst lastRow)
                        )
            |> Array.ofSeq        
    
    colBoundaries

let moveStateTurn (state:State) (direction:TurnDirection) : State =
    let newFacing =
        match state.currentFacing with
            | Left ->
                match direction with
                    | Clockwise -> Up
                    | CounterClockwise -> Down
            | Right ->
                match direction with
                    | Clockwise -> Down
                    | CounterClockwise -> Up
            | Up ->
                match direction with
                    | Clockwise -> Right
                    | CounterClockwise -> Left
            | Down ->
                match direction with
                    | Clockwise -> Left
                    | CounterClockwise -> Right

    { state with currentFacing = newFacing }


// We recurse so we can move one cell at a time so we can check for walls
let rec moveStateForward (state:State) (distance:int) : State =
    if (distance = 0)
    then
        state
    else
        let (row, col) = state.currentCell
        let mutable newRow = row
        let mutable newCol = col

        match state.currentFacing with
            | Left ->
                newCol <- col - 1
                if (newCol < fst state.rowBoundaries[row])
                then
                    newCol <- snd state.rowBoundaries[row]

            | Right ->
                newCol <- col + 1
                if (newCol > snd state.rowBoundaries[row])
                then
                    newCol <- fst state.rowBoundaries[row]

            | Up -> 
                newRow <- row - 1
                if (newRow < fst state.colBoundaries[col])
                then
                    newRow <- snd state.colBoundaries[col]
            | Down ->
                newRow <- row + 1
                if (newRow > snd state.colBoundaries[col])
                then
                    newRow <- fst state.colBoundaries[col]

        let newState =
            if (state.grid.[newRow, newCol] = Wall)
            then
                state    
            else
                { state with currentCell = (newRow, newCol) }

        moveStateForward newState (distance - 1)

let rec moveState (state:State) : State =
    match state.remainingActions with
        | [] -> state
        | head :: tail ->
            match head with
                | Move distance ->
                    let newState = moveStateForward state distance
                    moveState { newState with remainingActions = tail }
                | Turn direction ->
                    let newState = moveStateTurn state direction
                    moveState { newState with remainingActions = tail }

let parseIntoModel (lines:string[]) : unit =
    let l = lines.Length
    let top = lines[0..(l - 3)]
    let bottom = lines[l - 1]

    printfn "Parsed"
    let grid = parseGrid top
    //displayGrid grid

    let actions = parseActions bottom
    //printfn "Actions:\n%A" actions

    printfn "Grid is %d rows x %d cols" (Array2D.length1 grid) (Array2D.length2 grid)

    let rowBoundaries = getRowBoundariesOfGrid grid
    let colBoundaries = getColBoundariesOfGrid grid
    //printfn "%A" rowBoundaries
    //printfn ""
    //printfn "%A" colBoundaries

    let startingRow = 0
    let startingCol =
        seq { 0 .. Array2D.length2 grid}
            |> Seq.tryFind (fun col -> grid.[startingRow, col] = Tile)
            |> Option.defaultValue 0

    let state = {
        grid = grid
        rowBoundaries = rowBoundaries
        colBoundaries = colBoundaries
        remainingActions = List.ofSeq actions
        currentFacing = Right
        currentCell = (startingRow, startingCol)
    }

    let finalState = moveState state

    let row = 1 + fst finalState.currentCell
    let col = 1 + snd finalState.currentCell
    let facing = match finalState.currentFacing with
                    | Right -> 0
                    | Down -> 1
                    | Left -> 2
                    | Up -> 3

    let score = 1000 * row + 4 * col + facing
    printfn "Final score: %d" score
    ()

let solve =
    // let lines = Common.getSampleDataAsArray 2022 22
    let lines = Common.getChallengeDataAsArray 2022 22
    //printAllLines lines
    parseIntoModel lines
    ()
