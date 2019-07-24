module Year2018Day15

open System
open System.IO
open Common
open System.Text.RegularExpressions

type PlayerType = Elf | Goblin

type Player = {
    playerType: PlayerType;
    power: int;
    hitPoints: int;
    row: int;
    col: int;
}

type Cell =
      Empty
    | Occupied of Player
    | Wall

let getPlayerLetter (playerType: PlayerType) : char =
    match playerType with
        | Elf -> 'E'
        | Goblin -> 'G'

let printPlayers (players: Player list) : unit =
    players
        |> List.iter (fun p ->
                        printfn "%c at [%d,%d] with hit points = %d"
                            (getPlayerLetter p.playerType) p.row p.col p.hitPoints)

let mapCellToChar (c:Cell) : char =
    match c with
        | Empty -> '.'
        | Wall -> '#'
        | Occupied unit ->
            match unit.playerType with
                | Elf -> 'E'
                | Goblin -> 'G'

let printGrid grid =
    Common.printGrid grid mapCellToChar

let buildInitialGame (lines:string[]) : Cell[,] =

    let rows = lines.Length
    let cols = lines |> Array.map (fun line -> line.Length) |> Array.max
    let game = Array2D.init rows cols (fun row col ->
                                        match lines.[row].[col] with
                                            | '.' -> Empty
                                            | '#' -> Wall
                                            | 'E' -> Occupied {
                                                            playerType = Elf;
                                                            hitPoints = 200;
                                                            power = 3;
                                                            row = row;
                                                            col = col;
                                                        }
                                            | 'G' -> Occupied {
                                                            playerType = Goblin;
                                                            hitPoints = 200;
                                                            power = 3;
                                                            row = row;
                                                            col = col;
                                                        }
                                            | _ -> failwith "Unexpected character"
                                        )

    game

let getIndexList (game:Cell[,]) : (int * int) list =
    let rows = Array2D.length1 game
    let cols = Array2D.length2 game
    [for r in 0..rows-1 do for c in 0..cols - 1 do yield (r,c)]

let getPlayers (game:Cell[,]) =
    getIndexList game
        |> List.choose (fun (r,c) ->
                            match game.[r,c] with
                                | Occupied player -> Some player
                                | _ -> None)

let solve() =
    printfn "Day 15"
    //let testdata = Common.getChallengeDataAsArray 2018 15
    let testdata = Common.getSampleDataAsArray 2018 15
    //dump "data" testdata
    let game = buildInitialGame testdata

    printGrid game

    printPlayers (getPlayers game)
    ()
