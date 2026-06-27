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
    colors: ColorList
    state: TubeState
}

let EmptyTube = { colors = [|None; None; None; None|]; state = Empty }

type Game =
    {
        moveCount: int
        tubes: Tube[]
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

let mapLineToTube (line:string) : Tube =
    let colors = line |> Seq.map letterToColor |> Seq.map Some |> Array.ofSeq
    let state = tubeState colors
    {
        colors = colors; state = state
    }

let playGame (game: Game) : unit =
    ()

let initGame (lines:string[]) : Game =
    let tubes = lines |> Array.map mapLineToTube
    let extraCount = TUBE_COUNT - tubes.Length
    let emptyTubes = seq { 1 .. extraCount } 
                                                    |> Seq.map (fun _ -> EmptyTube)
                                                    |> Array.ofSeq
    let gameTubes = Array.concat [| tubes; emptyTubes |]
    let game = { moveCount = 0; tubes = gameTubes}
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