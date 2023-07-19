module Year2022Day24_Part1

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

type MoveableBlizzard =
    {
        initialPosition: Blizzard
        getFutureLocation: int -> (int * int)
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
        horizontalSnowCycleLength: int
        verticalSnowCycleLength: int
        blizzards: Blizzard[]
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

(* The initial postion of a blizzard is considered
    time zero.  The future position of the blizzard
    at time t must account for the cylcle lengths and
    the fact that the walls don't count.

    So for a storm moving West -- with an col delta of -1
    we must:
    - First subtract 1 from the storms current column
        to make it 0 based.
    - Then subtract t from that location to see how
        far it moved
    - Take's it's modulo cycle length
    - Add back the 1 to make it 1-based again
*)
let createMoveableBlizzard
            (colFunc: int -> (int * int) -> int -> (int * int))
            (rowFunc: int -> (int * int) -> int -> (int * int))
            (blizzard:Blizzard) : MoveableBlizzard =
            
    let locator (t:int) =
        match blizzard.motionDelta with
            | (0, -1) | (0, 1) ->
                let dc = snd blizzard.motionDelta
                colFunc dc blizzard.position t

            | (-1, 0) | (1, 0) ->
                let dr = fst blizzard.motionDelta
                rowFunc dr blizzard.position t
            | _ ->
                failwith "Unsupported movement delta"
    
    let moveableStorm =
        {
            initialPosition = blizzard
            getFutureLocation = locator
        }

    moveableStorm

let parseGridIntoState (grid:Cell[,]) : State =
    let rowCount = Array2D.length1 grid
    let colCount = Array2D.length2 grid

    // Collect all blizzards and rather than deal with them
    // in the grid, we should be able to determine their
    // positions at any time t given the cycle length

    // Don't include the border walls since the blizzards
    // wrap around them

    let horizontalSnowCycleLength = colCount - 2
    let verticalSnowCycleLength = rowCount - 2
    let mutable blizzards = []

    grid |> Array2D.iter (fun cell ->
                            match cell with
                                | Snow bizzard ->
                                    blizzards <- bizzard :: blizzards
                                | _ -> ()
                          ) 
   
    let vBlizzards = blizzards |> List.rev |> Array.ofList

    let colFunc (dc:int) (loc:int * int) (t:int) : (int * int)=
        let (row, col) = loc
        (row, ((col - 1) + dc) % horizontalSnowCycleLength + 1)

    let rowFunc (dr:int) (loc:int * int) (t:int) : (int * int) =
        let (row, col) = loc
        (((row - 1) + dr) % verticalSnowCycleLength + 1, col)


        
        
    let factory = createMoveableBlizzard colFunc rowFunc

    let moveableBlizzards = 
        vBlizzards |> Array.map factory
    {
        grid = grid
        rows = rowCount
        cols = colCount
        horizontalSnowCycleLength = horizontalSnowCycleLength
        verticalSnowCycleLength = verticalSnowCycleLength
        blizzards = vBlizzards
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
    printfn "State: %A" state
    printfn "Start at: %A and finish at %A" state.start state.finish

    //let path = solveStateSimple state
    //printfn "Path:\n%A" path
    ()
