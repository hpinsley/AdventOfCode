module ColorPuzzle

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

let TUBE_COUNT = 12

type Color = 
    | Blue
    | Cyan
    | Green
    | Lavender
    | Magenta
    | Orange
    | Purple
    | Red
    | White
    | Yellow

type TubeState =
    | Filled
    | Mixed
    | Empty

type ColorList = (Color option)[]

type Tube = {
    index: int
    colors: ColorList
    state: TubeState
}

type ColorCapacity = 
    | Upto of (int * Color)
    | FourOfAnyColor
    | NoCapacity

type Move = MoveTubes of (int * int * Color)

let EmptyTube = { index = -1; colors = [|None; None; None; None|]; state = Empty }

type Game =
    {
        moveCount: int
        tubes: Tube[]
        moveList: Move list
    }


let allSameColor (colors: Color[]) : bool =
    let colorToMatch= colors[0]
    not (Seq.exists (fun c -> c <> colorToMatch) colors)

let tubeState (colors: ColorList) : TubeState =
    let filled = colors |> Array.choose id
    match filled.Length with
        | 0 -> Empty
        | 4 -> if (allSameColor filled) then Filled
               else Mixed
        | _ -> Mixed
let getTubeCapacity (colors: ColorList) : ColorCapacity =
    match tubeState colors with
        | Empty -> FourOfAnyColor
        | Filled -> Upto (4, Option.get colors[0])
        | Mixed -> 
            let actualColors = colors |> Array.choose id
            let bottomColor = actualColors[actualColors.Length - 1]
            let bottomStreak = actualColors |> Array.rev |> Array.takeWhile (fun c -> c = bottomColor)
            Upto (bottomStreak.Length, bottomColor)

let gameSolved (game: Game) : bool =
    false

let letterToColor (c: char) : Color =
    match c with
        | 'B' -> Blue
        | 'C' -> Cyan
        | 'G' -> Green
        | 'L' -> Lavender
        | 'M' -> Magenta
        | 'O' -> Orange
        | 'P' -> Purple
        | 'R' -> Red
        | 'W' -> White
        | 'Y' -> Yellow
        | _ -> raise(Exception("No such color"))

let mapLineToTube (index: int) (line:string) : Tube =
    let colors = line |> Seq.map letterToColor |> Seq.map Some |> Array.ofSeq
    let state = tubeState colors
    {
        index = index;
        colors = colors; 
        state = state
    }

    

let findMoves (game: Game) : Move list =
    let capacity = game.tubes 
                        |> Seq.map (fun t -> t.colors)
                        |> Seq.map getTubeCapacity
                        |> Array.ofSeq
    []

let playGame (game: Game) : unit =
    let moves = findMoves game
    ()

let initGame (lines:string[]) : Game =
    let tubes = lines |> Array.mapi mapLineToTube
    let extraCount = TUBE_COUNT - tubes.Length
    let emptyTubes = seq { 1 .. extraCount } 
                                                    |> Seq.map (fun i -> { EmptyTube with index = tubes.Length + i - 1})
                                                    |> Array.ofSeq
    let gameTubes = Array.concat [| tubes; emptyTubes |]
    let game = { moveCount = 0; tubes = gameTubes; moveList = []}
    game


let solve =
    let stopWatch = Stopwatch.StartNew()

    let currentFolder = Environment.CurrentDirectory
    let puzzleInput = "cp-2026-06-25.txt"
    let inputFilespec = Path.Combine(currentFolder, "Challenges", "2026", puzzleInput)
    let lines = File.ReadAllLines inputFilespec
    printfn "%A" lines

    let game = initGame lines
    playGame game
    ()