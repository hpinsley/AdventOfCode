module Year2025Day7_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

type Cell =
    | Beam of (int * int)
    | Splitter of (int * int)
    | Empty of (int * int)

let IsBeam cell = match cell with
                        | Beam _ -> true
                        | _ -> false

let IsSplitter cell = match cell with
                                | Splitter _ -> true
                                | _ -> false

type Part1State = {
    board: Cell[,]
    beamList: Cell list
    splitCount: int
}

let processRow (state: Part1State) (row: int) : Part1State =
    let rows = Array2D.length1 state.board
    let cols = Array2D.length2 state.board
    if (row >= rows - 1) then
        state
    else
        let postRowState = seq { 0 .. cols - 1}
                            |> Seq.fold (fun s col ->
                                            let cell = s.board[row, col]
                                            let nextRow = row + 1

                                            if IsBeam cell then
                                                let cellBelow = s.board[nextRow, col]
                                                if IsSplitter cellBelow then
                                                    s
                                                else
                                                    // Beam move down
                                                    s.board[nextRow, col] <- Splitter (nextRow, col)
                                                    s
                                            else
                                                s

                                        ) state
        postRowState

let printBoard (board: Cell[,]) : unit =
    Common.printGrid board (fun c ->
                                                match c with
                                                    | Beam _ -> '|'
                                                    | Splitter _ -> '^'
                                                    | Empty _ -> ' '
                                            )

let part1 (board: Cell[,]) (startCol: int): unit =
    
    printBoard board
    
    let rows = Array2D.length1 board
    let cols = Array2D.length2 board

    printfn "\n%d rows and %d cols.  The start col index is %d\n" rows cols startCol

    let initialState = {
        board = board
        beamList = List.empty
        splitCount = 0
    }

    let finalState = seq { 0 .. rows - 2}
                        |> Seq.fold (fun s r ->
                                        s
                                    ) initialState
                      
    
    printfn "Final board"

    printBoard finalState.board
    
    printfn "Final split count is %d" finalState.splitCount

    
    ()

let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2025 7
    // let lines: string array = Common.getChallengeDataAsArray 2025 7

    let rows = lines.Length
    let cols = lines[0].Length
    let startCol = lines[0].IndexOf("S")

    let vData = Array2D.init rows cols (fun r c -> lines[r][c])
    let bData = vData |> Array2D.mapi  (fun row col c ->
                                                        match c with
                                                            | 'S' | '|' -> Beam (row, col)
                                                            | '^' -> Splitter (row, col)
                                                            | '.' -> Empty (row, col)
                                                            | _ -> raise (Exception("Unexpected character"))

                                                     )
    // Common.printGrid vData id

    part1 bData startCol

 

    ()