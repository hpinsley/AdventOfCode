module Year2018Day15

open System
open System.IO
open Common
open System.Text.RegularExpressions

//type UnitType = Elf | Goblin
type Unit =
      Elf of int * int
    | Goblin of int * int

type Cell =
      Empty
    | Occupied of Unit
    | Wall

let buildInitialGame (lines:string[]) : Cell[,] =
    let t = 3,200
    let e = Elf t

    let rows = lines.Length
    let cols = lines |> Array.map (fun line -> line.Length) |> Array.max
    let game = Array2D.init rows cols (fun row col ->
                                        match lines.[row].[col] with
                                            | '.' -> Empty
                                            | '#' -> Wall
                                            | 'E' -> Occupied (Elf (3,200))
                                            | 'G' -> Occupied (Goblin (3,200))
                                            | _ -> failwith "Unexpected character"
                                        )

    game

let solve() =
    printfn "Day 15"
    let testdata = Common.getChallengeDataAsArray 2018 15
    //let testdata = Common.getSampleDataAsArray 2018 15
    dump "data" testdata
    let game = buildInitialGame testdata

    ()
