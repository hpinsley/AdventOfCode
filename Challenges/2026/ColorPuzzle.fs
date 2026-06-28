module ColorPuzzle

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

let TUBE_COUNT = 12
let INITIAL_EMPTY_COUNT = 2
let INITIAL_NON_EMPTY_COUNT = TUBE_COUNT - INITIAL_EMPTY_COUNT

type INDEX = int
type COUNT = int
type FROM_INDEX = INDEX
type TO_INDEX = INDEX

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
    index: INDEX
    colors: ColorList
    state: TubeState
}

type SourceTubeMovePossibility = 
    | Upto of (COUNT * Color)
    | NoneTubeIsEmpty

type SourcePartial = 
    | Take of COUNT * Color * INDEX

type TargeTubeCapacity = 
    | FourOfAnyColor of INDEX
    | AsManyAs of (INDEX * COUNT * Color)
    | NoneTubeIsFull of INDEX

type Move = MoveTubes of COUNT * Color * INDEX * INDEX

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
let getSourceTubeMovePossibility (colors: ColorList) : SourceTubeMovePossibility =
    let actualColors = colors |> Array.choose id
    match actualColors.Length with
        | 0 -> NoneTubeIsEmpty
        | _ ->
            let bottomColor = actualColors[actualColors.Length - 1]
            let bottomStreak = actualColors |> Array.rev |> Array.takeWhile (fun c -> c = bottomColor)
            Upto (bottomStreak.Length, bottomColor)

let getTargeTubeCapacity (index: INDEX) (colors: ColorList) : TargeTubeCapacity =
    let actualColors = colors |> Array.choose id
    match actualColors.Length with
        | 0 -> FourOfAnyColor index
        | 4 -> NoneTubeIsFull index
        | _ ->
            let bottomColor = actualColors[actualColors.Length - 1]
            AsManyAs (index, colors.Length - actualColors.Length, actualColors[actualColors.Length-1])

let gameSolved (source: SourceTubeMovePossibility seq) : bool =
    let completedTubes = source |> Seq.sumBy (fun stmp -> 
                                                        match stmp with
                                                            | Upto (count, _) -> 
                                                                if count = 4 then 1 else 0
                                                            | NoneTubeIsEmpty -> 0
                                                    )
    completedTubes = INITIAL_NON_EMPTY_COUNT

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

let generateAllPossibleMoves (sourceMovePossibilities:SourceTubeMovePossibility[]) (targetTubeCapacities:TargeTubeCapacity[]) : Move[] =
    let sourceMoves = sourceMovePossibilities |> Seq.mapi (fun index smp -> 
                                                                    match smp with
                                                                        | Upto (n, color) ->
                                                                            seq { n .. -1 .. 1} |> Seq.map (fun i -> Take (i, color, index))
                                                                        | NoneTubeIsEmpty -> Seq.empty
                                                                       )
                                                                    |> Seq.concat

    let pairs = Seq.allPairs sourceMoves targetTubeCapacities
                                                        |> Array.ofSeq

    let validMoves = pairs |> Seq.choose (fun (sp, ttc) ->
                                                    match sp with 
                                                        | Take (sourceCount, sourceColor, sourceIndex) ->
                                                            match ttc with
                                                                | NoneTubeIsFull _ -> None
                                                                | FourOfAnyColor targetIndex -> 
                                                                    Some (MoveTubes (sourceCount, sourceColor, sourceIndex, targetIndex))
                                                                | AsManyAs (targetIndex, targetCapacity, targetColor) ->
                                                                    if sourceColor = targetColor && sourceCount <= targetCapacity
                                                                    then
                                                                        Some (MoveTubes (sourceCount, sourceColor, sourceIndex, targetIndex))
                                                                    else
                                                                        None
                                                    )
    Array.empty


let playGame (game: Game) : Game =
    let sourceMovePossibilities = game.tubes 
                                    |> Seq.map (fun t -> t.colors)
                                    |> Seq.map getSourceTubeMovePossibility
                                    |> Array.ofSeq

    if gameSolved sourceMovePossibilities
    then
        game
    else
        let targetTubeCapacities = game.tubes 
                                        |> Seq.map (fun t -> t.colors)
                                        |> Seq.mapi getTargeTubeCapacity
                                        |> Array.ofSeq

        let possbileMoves = generateAllPossibleMoves sourceMovePossibilities targetTubeCapacities
        game

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