module Year2022Day22_Part2

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

type CubeLocation =
    {
        sector: int
        side: Side
        sideRow: int
        sideCol: int
    }

type Helpers =
    {
        sectorToSide: int ->Side
        sideToSector: Side -> int
        getCubeLocation: int -> int -> CubeLocation
        getGridLocation: Side -> int -> int -> int * int
    }

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
        helpers: Helpers
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

let buildHelpers (grid:GridCellType[,]) (sideLength:int) (sectorMap:Side option list) : Helpers =
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

    let sideToSectorMap =
        sectorMap
            |> List.mapi (fun sectorNumber sideOption -> (sectorNumber, sideOption))
            |> List.choose (fun (sectorNumber, sideOption) ->
                                match sideOption with
                                    | None -> None
                                    | Some side -> Some (side, sectorNumber)
                            )
            |> Map.ofList

    printfn "%A" sideToSectorMap

    let sectorToSide (sectorNumber:int) : Side =
        match sectorMap[sectorNumber] with
            | Some side -> side
            | None -> failwith "No side for sector"
            
    let sideToSector (side:Side) : int =
        sideToSectorMap[side]

    let getGridLocation (side:Side) (sideRow:int) (sideCol:int) : (Row * Col) =
        let sector = sideToSector side
        let row = (sector / colSectors) * sideLength + sideRow
        let col = (sector % colSectors) * sideLength + sideCol
        (row, col)

    let getCubeLocation (row:int) (col:int) : CubeLocation =
        let sector = (row / sideLength) * colSectors + col / sideLength
        let side = sectorToSide sector
        let sideRow = row % sideLength
        let sideCol = col % sideLength
        { sector = sector; side = side; sideRow = sideRow; sideCol = sideCol }

    let helpers = {
        sectorToSide = sectorToSide
        sideToSector = sideToSector
        getCubeLocation = getCubeLocation
        getGridLocation = getGridLocation
    }
    
    helpers

let parseIntoModel (lines:string[]) (sideLength:int) (sectorMap:Side option list): State =
    let l = lines.Length
    let top = lines[0..(l - 3)]
    let bottom = lines[l - 1]

    printfn "Parsed"
    let grid = parseGrid top
    //displayGrid grid

    let actions = parseActions bottom
    //printfn "Actions:\n%A" actions


    let startingRow = 0
    let startingCol =
        seq { 0 .. Array2D.length2 grid}
            |> Seq.tryFind (fun col -> grid.[startingRow, col] = Tile)
            |> Option.defaultValue 0

    let helpers = buildHelpers grid sideLength sectorMap

    let state = {
        grid = grid
        remainingActions = List.ofSeq actions
        currentFacing = Facing.Right
        currentCell = (startingRow, startingCol)
        helpers = helpers
    }

    state

let solve =
    let lines = Common.getSampleDataAsArray 2022 22
    // These have to change depending on which data set you are solving
    let sideLength = 4
    let sectorMap = [
        None; None; Some Side.Top; None;
        Some Side.Back; Some Side.Left ; Some Side.Front; None;
        None; None; Some Side.Bottom; Some Side.Right
    ]
    
    //let lines = Common.getChallengeDataAsArray 2022 22
    //// These have to change depending on which data set you are solving
    //let sideLength = 50
    //let sectorMap = [
    //    None; Some Side.Back; Some Side.Right; 
    //    None; Some Side.Top; None; 
    //    Some Side.Left; Some Side.Front; None; 
    //    Some Side.Bottom; None; None 
    //]

    //printAllLines lines
    let state = parseIntoModel lines sideLength sectorMap

    while true do
        try
            printf "Enter a grid coord pair: "
            let s = Console.ReadLine()
            let parts = s.Split(' ')
            let v = parts |> Array.map parseInt
            let r = v[0]
            let c = v[1]
            let cubeLocation = state.helpers.getCubeLocation r c
            printfn "(%d,%d) = \n%A" r c cubeLocation

            let gr,gc = state.helpers.getGridLocation cubeLocation.side cubeLocation.sideRow cubeLocation.sideCol
            printfn "Reversed: (%d,%d)" gr gc
        with ex ->
            printfn "Error %s" ex.Message
    ()
