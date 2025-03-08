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
    knotPositions: (int * int)[]
    tailVisits: Set<int * int>[]
}


let moveOneStep (direction:Direction) (state:State) : State =
    let (r, c) = state.knotPositions[0]

    state.knotPositions[0] <- match direction with
                                        | Up -> (r - 1,c)
                                        | Down -> (r + 1, c)
                                        | Right -> (r, c + 1)
                                        | Left -> (r, c - 1)


    for headIndex = 0 to (state.knotPositions.Length - 2) do
        let tailIndex = headIndex + 1
        // If the tail is not adjacent to head we have to move it
        let (headRow, headCol) = state.knotPositions[headIndex]
        let (tailRow, tailCol) = state.knotPositions[tailIndex]

        let rowDiff = headRow - tailRow
        let colDiff = headCol - tailCol

        if (abs rowDiff > 1 || abs colDiff > 1)
        then
            let row = tailRow + sign rowDiff
            let col = tailCol + sign colDiff
            state.knotPositions[tailIndex] <- (row, col)
            state.tailVisits[tailIndex] <- Set.add (row, col) state.tailVisits[tailIndex]

    state

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
    
    // let knotCount = 2       // Part 1
    let knotCount = 10       // Part 2

    let initialState = {
        knotPositions = Array.create knotCount startAt
        tailVisits = Array.create knotCount (Set.ofArray [| startAt|])
    }
    
    printfn "Initial state:\n%A" initialState
    let finalState = moves |> Array.fold processInstruction initialState
    printfn "Final state:\n%A" finalState

    printfn "Tail visit count: %d" (Array.last finalState.tailVisits).Count