module Year2025Day1_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let DIAL_SIZE = 100
let STARTING_NUMBER = 50

type State =
    {
        currentNumber: int
        moveCount: int
        zeroCount: int
        zeroClicks: int
    }

type Rotation = (int * int)

let processPart1Rotation (state: State) (rotation: Rotation) : State =

    let (direction, magnitude) = rotation
    let currentNumber = (((state.currentNumber + direction * magnitude) % DIAL_SIZE) + DIAL_SIZE) % DIAL_SIZE

    { state with 
        currentNumber = currentNumber;
        moveCount = state.moveCount + 1;
        zeroCount = if currentNumber = 0 then state.zeroCount + 1 else state.zeroCount
    }

let processPart2Rotation (state: State) (rotation: Rotation) : State =
    let (direction, magnitude) = rotation
 
    let turns = magnitude / DIAL_SIZE
    let currentNumber = (((state.currentNumber + direction * magnitude) % DIAL_SIZE) + DIAL_SIZE) % DIAL_SIZE

    let toAdd = if currentNumber = 0
                        then
                            if state.currentNumber = 0
                            then
                                0
                            else
                                1
                        else
                            if direction < 0
                            then
                                if state.currentNumber <> 0 && currentNumber > state.currentNumber then 1 else 0
                            else
                                if state.currentNumber <> 0 && currentNumber < state.currentNumber then 1 else 0
    
    let zeroClicks = turns + toAdd         

    let newState = { state with 
                                    currentNumber = currentNumber;
                                    moveCount = state.moveCount + 1;
                                    zeroCount = if currentNumber = 0 then state.zeroCount + 1 else state.zeroCount
                                    zeroClicks = state.zeroClicks + zeroClicks
                                }

    // printfn "%A %A %A\n=============" state rotation newState
    newState

let parseRotation (instruction:string) : Rotation =
    let m = Regex("(R|L)(\d+)").Match(instruction)
    if (not m.Success)
    then
        failwith "Bad match"

    let direction = if m.Groups[1].Value = "R" then 1 else -1
    let steps = int m.Groups[2].Value
    (direction, steps)

let part1 (rotations:Rotation[]) : State =
    let finalState = rotations |> Array.fold processPart1Rotation { currentNumber = STARTING_NUMBER; moveCount = 0; zeroCount = 0; zeroClicks = 0 }
    finalState

let part2 (rotations:Rotation[]) : State =
    let finalState = rotations |> Array.fold processPart2Rotation { currentNumber = STARTING_NUMBER; moveCount = 0; zeroCount = 0; zeroClicks = 0 }
    finalState

let solve =
    // let lines = Common.getSampleDataAsArray 2025 1
    let lines = Common.getChallengeDataAsArray 2025 1
    printfn "Input text: %A" lines
    let rotations = lines |> Array.map parseRotation

    // let part1Result = part1 rotations
    // printfn "Part1 result: %A" part1Result

    let part2Result = part2 rotations
    printfn "Part2 result: %A" part2Result

    ()