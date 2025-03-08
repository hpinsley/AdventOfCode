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

// This is a map of where you are and the direction you are moving
// and where you end up (assuming a single cell movement)

let rules =
    [|
        ((Side.Top, Facing.Left), (Side.Left, Facing.Down))
        ((Side.Top, Facing.Right), (Side.Right, Facing.Down))
        ((Side.Top, Facing.Up), (Side.Back, Facing.Down))
        ((Side.Top, Facing.Down), (Side.Front, Facing.Down))

        ((Side.Bottom, Facing.Left), (Side.Left, Facing.Up))
        ((Side.Bottom, Facing.Right), (Side.Right, Facing.Up))
        ((Side.Bottom, Facing.Up), (Side.Front, Facing.Up))
        ((Side.Bottom, Facing.Down), (Side.Back, Facing.Up))
    
        ((Side.Left, Facing.Left), (Side.Back, Facing.Right))
        ((Side.Left, Facing.Right), (Side.Front, Facing.Right))
        ((Side.Left, Facing.Up), (Side.Top, Facing.Right))
        ((Side.Left, Facing.Down), (Side.Bottom, Facing.Right))
        
        ((Side.Right, Facing.Left), (Side.Front, Facing.Left))
        ((Side.Right, Facing.Right), (Side.Back, Facing.Left))
        ((Side.Right, Facing.Up), (Side.Top, Facing.Left))
        ((Side.Right, Facing.Down), (Side.Bottom, Facing.Left))
    
        ((Side.Front, Facing.Left), (Side.Left, Facing.Left))
        ((Side.Front, Facing.Right), (Side.Right, Facing.Right))
        ((Side.Front, Facing.Up), (Side.Top, Facing.Up))
        ((Side.Front, Facing.Down), (Side.Bottom, Facing.Down))
    
        ((Side.Back, Facing.Left), (Side.Right, Facing.Left))
        ((Side.Back, Facing.Right), (Side.Left, Facing.Right))
        ((Side.Back, Facing.Up), (Side.Top, Facing.Up))
        ((Side.Back, Facing.Down), (Side.Bottom, Facing.Down))

    |]
        |> Map.ofArray

type State =
    {
        sideLength: int
        grid: GridCellType[,]
        remainingActions: Action list
        currentFacing: Facing
        currentLocation: CubeLocation
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

    let startingLocation = helpers.getCubeLocation startingRow startingCol 

    let state = {
        sideLength = sideLength
        grid = grid
        remainingActions = List.ofSeq actions
        currentFacing = Facing.Right
        helpers = helpers
        currentLocation = startingLocation
    }

    state

let testHelpers (state:State) : unit =
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

let moveStateTurn (state:State) (direction:TurnDirection) : State =
    let newFacing =
        match state.currentFacing with
            | Facing.Left ->
                match direction with
                    | Clockwise -> Up
                    | CounterClockwise -> Facing.Down
            | Facing.Right ->
                match direction with
                    | Clockwise -> Facing.Down
                    | CounterClockwise -> Facing.Up
            | Facing.Up ->
                match direction with
                    | Clockwise -> Facing.Right
                    | CounterClockwise -> Facing.Left
            | Facing.Down ->
                match direction with
                    | Clockwise -> Facing.Left
                    | CounterClockwise -> Facing.Right

    { state with currentFacing = newFacing }


// We recurse so we can move one cell at a time so we can check for walls
let rec moveStateForward (state:State) (distance:int) : State =
    //if (distance = 0)
    //then
    //    state
    //else
    //    let currentLocation = state.currentLocation
    //    let (newSide, newFacing) = rules[(currentLocation.side, state.currentFacing)]
    //    let (row, col) = currentLocation.sideRow, currentLocation.sideCol
    //    let newSideOpt =
    //        match state.currentFacing with
    //                | Facing.Left ->
    //                    if (col = 0) then Some (newSide, newFacing) else None
    //                | Facing.Right ->
    //                    if (col = (state.sideLength - 1)) then Some (newSide, newFacing) else None
    //                | Facing.Up -> 
    //                    if (row = 0) then Some (newSide, newFacing) else None
    //                | Facing.Down ->
    //                    if (row = (state.sideLength - 1)) then Some (newSide, newFacing) else None

        (*
            Compute the new side position.  If we are moving to a new
            side, it gets tricky ;-)

            For example, if we are on the top in at (1,3) -- which is 
            the right edge, and move right, we end up on the right side
            at (0, 2).  We also know we are pointing down.  So I think
            that's why the row would be 0.  But I think the new column
            is derived from the original side.  I think it would
            be sideLength - 1 - row.

            Another example.  If we are front at (3,2) -- which is
            the bottom edge, and move down, we end up on the bottom
            at (3,1).  We also know we are pointing up.  So I think
            that's why the row would be 3 (since more up movement decreases
            the row).  But I think the new column
            is derived from the the original side.  It think it would
            be ??

            Actually, now I'm thinking its worse than this.  If we want
            to maintain the integrity of the original map, I think we
            have to deal with the fact that folding the map -- the rows
            become columns and the columns become rows.  Look at (in the
            rotated sample how 5 Left folds next to 6 Top.  The rows
            and columns flip.

            Update: I read the comments in /r/adventofcode and coming
            up with a general solution is pretty hard.  Most people
            just special cased it.  I don't think I will do that but will
            move on to day 23

        *)
        
                
                
        //let (gridRow, gridCol) = state.helpers.getGridLocation newSide newRow newCol

        //let newState =
        //    if (state.grid.[gridRow, gridCol] = Wall)
        //    then
        //        state    
        //    else
        //        let newLocation = state.helpers.getCubeLocation gridRow gridCol

        //        { state with 
        //                currentFacing = newFacing
        //                currentLocation = newLocation
        //        }

        //moveStateForward newState (distance - 1)
    
    state   // This is obviously wrong; just keeping the compiler happy

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


let buildTestState (state:State) (gridRow:int) (gridCol:int) (facing:Facing) : State =
    let cubeLocation = state.helpers.getCubeLocation gridRow gridCol
    { state 
        with currentFacing = facing; 
             currentLocation = cubeLocation;
             remainingActions = [Move 1]}

let solve =

    let lines = Common.getSampleDataAsArray 2022 22
    // These have to change depending on which data set you are solving
    let sideLength = 4

    // Original
    //let sectorMap = [
    //    None; None; Some Side.Top; None;
    //    Some Side.Back; Some Side.Left ; Some Side.Front; None;
    //    None; None; Some Side.Bottom; Some Side.Right
    //]
    
    // Rotated to match the sample (I think)
    let sectorMap = [
        None; None; Some Side.Back; None;
        Some Side.Bottom; Some Side.Left ; Some Side.Top; None;
        None; None; Some Side.Front; Some Side.Right
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
    // printfn "%A" state

    let testState = buildTestState state 5 11 Facing.Right
    let finalState = moveState testState
    
    // printfn "Final state: %A" finalState
    let gridRow, gridCol = finalState.helpers.getGridLocation finalState.currentLocation.side finalState.currentLocation.sideRow finalState.currentLocation.sideCol

    let row = 1 + gridRow
    let col = 1 + gridCol

    let facing = match finalState.currentFacing with
                    | Facing.Right -> 0
                    | Facing.Down -> 1
                    | Facing.Left -> 2
                    | Facing.Up -> 3

    let score = 1000 * row + 4 * col + facing
    printfn "Final score: %d" score
    ()
