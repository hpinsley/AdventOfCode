module Year2018Day15

open System
open System.IO
open Common
open System.Text.RegularExpressions

type Unit =
      Elf of int * int
    | Goblin of int * int

type Cell =
      Empty
    | Occupied of Unit
    | Wall

let mapCellToChar (c:Cell) : char =
    match c with
        | Empty -> '.'
        | Wall -> '#'
        | Occupied unit ->
            match unit with
                | Elf _ -> 'E'
                | Goblin _ -> 'G'

let printGrid grid =
    Common.printGrid grid mapCellToChar

let buildInitialGame (lines:string[]) : Cell[,] =

    let rows = lines.Length
    let cols = lines |> Array.map (fun line -> line.Length) |> Array.max
    let game = Array2D.init rows cols (fun row col ->
                                        match lines.[row].[col] with
                                            | '.' -> Empty
                                            | '#' -> Wall
                                            | 'E' -> Occupied (Elf (3, 200))
                                            | 'G' -> Occupied (Goblin (3, 200))
                                            | _ -> failwith "Unexpected character"
                                        )

    game

let getIndexList (game:Cell[,]) : (int * int) list =
    let rows = Array2D.length1 game
    let cols = Array2D.length2 game
    [for r in 0..rows-1 do for c in 0..cols - 1 do yield (r,c)]

let getUnits (game:Cell[,]) =
    getIndexList game
        |> List.choose (fun (r,c) ->
                            match game.[r,c] with
                                | Occupied unit -> Some unit
                                | _ -> None)

let solve() =
    printfn "Day 15"
    //let testdata = Common.getChallengeDataAsArray 2018 15
    let testdata = Common.getSampleDataAsArray 2018 15
    //dump "data" testdata
    let game = buildInitialGame testdata

    printGrid game

    printfn "Units: %A" (getUnits game)
    ()
