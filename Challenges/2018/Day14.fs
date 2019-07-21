module Year2018Day14

open System
open System.IO
open Common
open System.Text.RegularExpressions

type Elf = {
    currentRecipeIndex: int;
}

type GameState = {
    elves: Elf list;
    recipeBoard: int[];
    recipeCount: int;
    initialRecipeCount: int;
    toGenerateRecipeCount: int;
    roundsCompleted: int;
    target: (int list) option
}

let matchesPattern (pattern:int list) (recipeBoard:int[]) (recipeCount:int) : bool =
    if (pattern.Length > recipeCount)
    then
        false
    else
        let startingIndex = recipeCount - pattern.Length

        pattern
            |> Seq.mapi (fun i r -> r = recipeBoard.[startingIndex + i])
            |> Seq.forall id


let rec playRound1 (state:GameState) : GameState =
    // printfn "_______________\n%A" state

    if (state.roundsCompleted % 100000 = 0) then printfn "Round %d" state.roundsCompleted

    let scoreSum = List.sumBy (fun elf -> state.recipeBoard.[elf.currentRecipeIndex]) state.elves
    let digits = if scoreSum < 10 then [scoreSum] else [scoreSum / 10; scoreSum % 10]
    let stateWithNewRecipes =
        digits
            |> List.fold
            (fun s recipeScore ->

                s.recipeBoard.[s.recipeCount] <- recipeScore
                let _state = { s with recipeCount = s.recipeCount + 1}
                _state
            ) state

    let elves = stateWithNewRecipes.elves
                    |> List.map (fun elf ->
                                    let score = stateWithNewRecipes.recipeBoard.[elf.currentRecipeIndex]
                                    let toMove = 1 + score
                                    { currentRecipeIndex = (elf.currentRecipeIndex + toMove) % stateWithNewRecipes.recipeCount }
                                )
    let stateWithElves = { stateWithNewRecipes with elves = elves }

    let state_ = {stateWithElves with roundsCompleted = stateWithElves.roundsCompleted + 1}

    if (state_.recipeCount >= (state_.initialRecipeCount + state_.toGenerateRecipeCount))
    then
        state_
    else
        playRound1 state_

let rec playRound2 (state:GameState) : GameState =
    // printfn "_______________\n%A" state

    if (state.roundsCompleted % 100000 = 0) then printfn "Round %d" state.roundsCompleted

    let scoreSum = List.sumBy (fun elf -> state.recipeBoard.[elf.currentRecipeIndex]) state.elves
    let digits = if scoreSum < 10 then [scoreSum] else [scoreSum / 10; scoreSum % 10]
    let stateWithNewRecipes =
        digits
            |> List.fold
            (fun s recipeScore ->

                s.recipeBoard.[s.recipeCount] <- recipeScore
                let _state = { s with recipeCount = s.recipeCount + 1}
                match _state.target with
                        | None -> ()
                        | Some pattern ->
                            if (matchesPattern pattern _state.recipeBoard _state.recipeCount)
                            then
                                printfn "EARLY MATCH at %d" (_state.recipeCount - pattern.Length)
                                failwith "Got an early match"
                _state
            ) state

    let elves = stateWithNewRecipes.elves
                    |> List.map (fun elf ->
                                    let score = stateWithNewRecipes.recipeBoard.[elf.currentRecipeIndex]
                                    let toMove = 1 + score
                                    { currentRecipeIndex = (elf.currentRecipeIndex + toMove) % stateWithNewRecipes.recipeCount }
                                )
    let stateWithElves = { stateWithNewRecipes with elves = elves }

    let state_ = {stateWithElves with roundsCompleted = stateWithElves.roundsCompleted + 1}

    match state_.target with
        | None -> failwith "Expect a target"
        | Some pattern ->
            if (matchesPattern pattern state_.recipeBoard state_.recipeCount)
            then
                state_
            else
                playRound2 state_

let solvePartOne (state:GameState) : unit =
    printfn "Starting part one"
    printfn "%A" state
    let finalState =  playRound1 state
    let answer = finalState.recipeBoard
                    |> Seq.skip finalState.initialRecipeCount
                    |> Seq.take finalState.toGenerateRecipeCount
                    |> Seq.map string
                    |> String.concat ""

    printfn "%A" finalState
    printfn "The answer is [%s]" answer

let solvePartTwo (state:GameState) : unit =
    printfn "Starting part two"
    printfn "%A" state
    let finalState =  playRound2 state
    let answer = finalState.recipeBoard
                    |> Seq.skip finalState.initialRecipeCount
                    |> Seq.take finalState.toGenerateRecipeCount
                    |> Seq.map string
                    |> String.concat ""

    printfn "%A" finalState
    printfn "The answer is [%s]" answer
    printfn "%d" (finalState.recipeCount - (List.length (Option.defaultValue [] finalState.target)))

let solve() =
    // let initalRecipeCount = 9
    // let target = Some [5;1;5;8;9]

    let initalRecipeCount = 768071
    let target = None

    let initalRecipeCount = 76807100
    let target = Some [7;6;8;0;7;1]

    let toGenerateRecipeCount = 10

    let dim = initalRecipeCount + toGenerateRecipeCount + 1;

    let initialState = {

        elves = [ { currentRecipeIndex = 0 }; { currentRecipeIndex = 1 }];
        recipeBoard = Array.init dim (fun index ->
                                        match index with
                                            | 0 -> 3
                                            | 1 -> 7
                                            | _ -> 0
                                    );
        recipeCount = 2;
        initialRecipeCount = initalRecipeCount;
        toGenerateRecipeCount = toGenerateRecipeCount;
        roundsCompleted = 0;
        target = target
    }

    //solvePartOne initialState
    solvePartTwo initialState
    ()
