module Year2022Day09_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions

type Direction =
    | Left | Right | Up | Down

type MoveInstruction =
    | Move of Direction * int

let letterToDirection (letter:char): Direction =
    match letter with
        | 'R' -> Right
        | 'L' -> Left
        | 'U' -> Up
        | 'D' -> Down
        | _ -> sprintf "Unknown letter %A" letter |> failwith

let lineToInstruction (line:string) : MoveInstruction =
    match line with
        | Common.ParseRegex "(.*) (.*)" [direction; steps] -> Move (letterToDirection direction[0], int(steps))

type State = {
    headPosition: int * int
    tailPosition: int * int
    tailVisits: Set<int * int>
}


let moveOneStep (direction:Direction) (state:State) : State =
    let (r, c) = state.headPosition

    let afterHeadMoves = { state with headPosition =
                                    match direction with
                                        | Up -> (r - 1,c)
                                        | Down -> (r + 1, c)
                                        | Right -> (r, c + 1)
                                        | Left -> (r, c - 1)
                         }

    // If the tail is not adjacent to head we have to move it
    let { State.headPosition = (headRow,headCol); State.tailPosition = (tailRow, tailCol) } = afterHeadMoves
    let rowDiff = headRow - tailRow
    let colDiff = headCol - tailCol

    if (abs rowDiff <= 1 && abs colDiff <= 1)
    then // Adjacent to head; tail does not need to move
        afterHeadMoves
    else // Tail always moves one step in the correct directions to maintain adjacency
        let row = tailRow + sign rowDiff
        let col = tailCol + sign colDiff
        { afterHeadMoves with 
            tailPosition = (row, col);
            tailVisits = Set.add (row, col) afterHeadMoves.tailVisits
        }

let rec processInstruction (state: State) (instruction:MoveInstruction) : State =
    let (direction, steps) = match instruction with
                                | Move (d, s) -> (d, s)

    if (steps = 0)
    then
        state
    else
        let stepState = moveOneStep direction state         // Move only once
        let nextInstruction = Move (direction, steps - 1)   // Recurse with one less step
        processInstruction stepState nextInstruction

let solve =
    // let lines = Common.getSampleDataAsArray 2022 09
    let lines = Common.getChallengeDataAsArray 2022 09

    printfn "%A" lines
    let moves = lines |> Array.map lineToInstruction
    let paired = Array.zip lines moves

    for p in paired do
        printfn "%A => %A" (fst p) (snd p)

    let startAt = (0,0)

    let initialState = {
        headPosition = startAt
        tailPosition = startAt
        tailVisits = Set.ofArray [| startAt|]
    }
    
    printfn "Initial state:\n%A" initialState
    let finalState = moves |> Array.fold processInstruction initialState
    printfn "Final state:\n%A" finalState

    printfn "Part 1: %d" finalState.tailVisits.Count