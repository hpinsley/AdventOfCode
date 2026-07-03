module ColorPuzzle

open System
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

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

type ColorList = (Color option)[]

type Tube = {
    index: INDEX
    colors: ColorList
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

let EmptyTube = { index = -1; colors = [|None; None; None; None|] }

type Game =
    {
        moveCount: int
        tubes: Tube[]
        moveList: Move list
    }


let allSameColor (colors: Color[]) : bool =
    let colorToMatch= colors[0]
    not (Seq.exists (fun c -> c <> colorToMatch) colors)

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

let getColorCount (colors:ColorList) : int =
    Array.choose id colors |> Array.length

// Solved when every tube is either empty or completely filled with one color
// (a streak of 4 spans the whole tube). Works for any number of filled tubes.
let gameSolved (source: SourceTubeMovePossibility seq) : bool =
    source |> Seq.forall (fun stmp ->
                                match stmp with
                                    | Upto (count, _) -> count = 4
                                    | NoneTubeIsEmpty -> true)

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
    {
        index = index;
        colors = colors; 
    }

let removeColorsFromTube (tube:Tube) (moveCount:int) : Tube =
    let colors = tube.colors
    let colorCount = getColorCount colors
    let reducedColorCount = colorCount - moveCount
    let paddingNullCount = 4 - reducedColorCount

    let reducedColors = Array.append 
                                colors[0..reducedColorCount - 1]
                                (Array.create paddingNullCount None)
    { tube with colors = reducedColors }

let addColorsToTube (tube:Tube) (color:Color) (moveCount:int) : Tube =
    let colors = tube.colors
    let colorCount = getColorCount colors
    let increasedColorCount = colorCount + moveCount
    let paddingNullCount = 4 - increasedColorCount

    let segment1 = if colorCount = 0 then Array.empty else colors[0..colorCount - 1]
    let segment2 = Array.create moveCount (Some color)
    let segment3 = Array.create paddingNullCount None

    let increasedColors = seq { segment1; segment2; segment3 } |> Array.concat

    { tube with colors = increasedColors }

let makeMove (game: Game) (move:Move) : Game =
    // type Move = MoveTubes of COUNT * Color * INDEX * INDEX
    match move with
        | MoveTubes (moveCount, color, fromIndex, toIndex) ->
            let sourceTube = game.tubes[fromIndex]
            let reducedSourceTube = removeColorsFromTube sourceTube moveCount

            let targetTube = game.tubes[toIndex]
            let increasedTargetTube = addColorsToTube targetTube color moveCount

            let updatedTubes = Array.copy game.tubes
            updatedTubes[fromIndex] <- reducedSourceTube
            updatedTubes[toIndex] <- increasedTargetTube
            
            { game with tubes = updatedTubes; moveCount = game.moveCount + 1; moveList = move :: game.moveList }

let generateAllPossibleMoves (sourceMovePossibilities:SourceTubeMovePossibility[]) (targetTubeCapacities:TargeTubeCapacity[]) (colorCounts:int[]) : Move seq =
    // A tube whose streak spans its entire contents is single-colored; pouring it
    // into an empty tube just relocates it, and a full one (streak of 4) is done.
    let isSingleColored = Array.map2 (fun smp count ->
                                            match smp with
                                                | Upto (streak, _) -> streak = count
                                                | NoneTubeIsEmpty -> false)
                                     sourceMovePossibilities colorCounts

    let sourceMoves = sourceMovePossibilities |> Seq.mapi (fun index smp ->
                                                                    match smp with
                                                                        | Upto (4, _) -> Seq.empty   // completed tube; never a useful source
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
                                                                    if targetIndex = sourceIndex || isSingleColored[sourceIndex]
                                                                    then
                                                                        None
                                                                    else
                                                                        Some (MoveTubes (sourceCount, sourceColor, sourceIndex, targetIndex))
                                                                | AsManyAs (targetIndex, targetCapacity, targetColor) ->
                                                                    if targetIndex <> sourceIndex && sourceColor = targetColor && sourceCount <= targetCapacity
                                                                    then
                                                                        Some (MoveTubes (sourceCount, sourceColor, sourceIndex, targetIndex))
                                                                    else
                                                                        None
                                                    )
    validMoves

let colorToLetter (c: Color) : char =
    match c with
        | Blue -> 'B'
        | Cyan -> 'C'
        | Green -> 'G'
        | Lavender -> 'L'
        | Magenta -> 'M'
        | Orange -> 'O'
        | Purple -> 'P'
        | Red -> 'R'
        | White -> 'W'
        | Yellow -> 'Y'

// Canonical key: tubes are interchangeable, so sorting their encodings makes
// permutations of the same position hash identically.
let gameToKey (game: Game) : string =
    game.tubes
        |> Array.map (fun t -> t.colors
                                |> Array.map (fun c -> match c with
                                                        | Some color -> colorToLetter color
                                                        | None -> '.')
                                |> System.String)
        |> Array.sort
        |> String.concat ""

// Breadth-first search: states are visited in move-count order, so the first
// solved state found uses the fewest moves.
let solveGame (initial: Game) : Game option =
    let visited = HashSet<string>()
    let queue = Queue<Game>()
    visited.Add(gameToKey initial) |> ignore
    queue.Enqueue initial

    let mutable solution = None
    while Option.isNone solution && queue.Count > 0 do
        let game = queue.Dequeue()

        let sourceMovePossibilities = game.tubes
                                        |> Array.map (fun t -> getSourceTubeMovePossibility t.colors)

        if gameSolved sourceMovePossibilities
        then
            solution <- Some game
        else
            let targetTubeCapacities = game.tubes
                                            |> Array.mapi (fun i t -> getTargeTubeCapacity i t.colors)
            let colorCounts = game.tubes
                                            |> Array.map (fun t -> getColorCount t.colors)

            let possibleMoves = generateAllPossibleMoves sourceMovePossibilities targetTubeCapacities colorCounts

            for move in possibleMoves do
                let nextGame = makeMove game move
                if visited.Add(gameToKey nextGame) then
                    queue.Enqueue nextGame

    solution


let initGame (lines:string[]) : Game =
    let tubes = lines |> Array.mapi mapLineToTube
    let extraCount = TUBE_COUNT - tubes.Length
    let emptyTubes = seq { 1 .. extraCount } 
                                                    |> Seq.map (fun i -> { EmptyTube with index = tubes.Length + i - 1})
                                                    |> Array.ofSeq
    let gameTubes = Array.concat [| tubes; emptyTubes |]
    let game = { moveCount = 0; tubes = gameTubes; moveList = []}
    game


// Entry point for callers: lines in puzzle-file format (one 4-letter tube per
// entry, e.g. "YWCB"); empty tubes are padded up to TUBE_COUNT by initGame.
let solvePuzzle (lines: string[]) : Game option =
    lines |> initGame |> solveGame