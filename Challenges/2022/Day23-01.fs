module Year2022Day23_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let ROUNDS_TO_PLAY = 10

type ElfInfo = {
    id: int
    mutable location: (int * int)
    mutable proposedLocation: (int * int) option
}

(* 
    If there is no Elf in the N, NE, or NW adjacent positions, the Elf proposes moving north one step.
    If there is no Elf in the S, SE, or SW adjacent positions, the Elf proposes moving south one step.
    If there is no Elf in the W, NW, or SW adjacent positions, the Elf proposes moving west one step.
    If there is no Elf in the E, NE, or SE adjacent positions, the Elf proposes moving east one step.
*)

let LookStategies =
    [|
        ([|(-1, 0); (-1, -1); (-1, 1)|], (-1, 0))
        ([|(1, 0); (1, -1); (1, 1)|], (1, 0))
        ([|(0, -1); (-1, -1); (1, -1)|], (0, -1))
        ([|(0, 1); (-1, 1); (1, 1)|], (0, 1))
    |]

type LookStrategy = {
    offsetsToCheck: (int * int)[]
    offsetToMove: (int * int)
}

type State = 
    {
        lookStrategies: LookStrategy[]
        elfCount: int
        currentRound: int
        roundsToPlay: int
        elves: ElfInfo[]
        occupiedLocations: Dictionary<(int * int), ElfInfo>
    }

let parseLines (lines:string[]) : (int * int) list =
    let parseLine (row:int) (line:string) : (int * int) seq =
       let occupied = line
                        |> Seq.mapi (fun col c -> match c with
                                                    | '#' -> Some (row, col)
                                                    | '.' -> None
                                                    | _ -> failwith "Invalid character"
                                    )
                        |> Seq.choose id
                        
       occupied
    let allRows = lines
                    |> Seq.mapi parseLine
                    |> Seq.concat
                    |> Seq.toList
    allRows

let buildState (occupied:(int * int) list) : State =
    let elves = occupied
                    |> Seq.mapi (fun id (row, col) -> { id = id; location = (row, col); proposedLocation = None })
                    |> Seq.toArray
    
    let lookStrategies =
        LookStategies
            |> Array.map (fun (offsets, offsetToMove) -> { offsetsToCheck = offsets; offsetToMove = offsetToMove })

    {
        lookStrategies = lookStrategies
        elfCount = elves.Length
        currentRound = 0
        roundsToPlay = ROUNDS_TO_PLAY
        elves = elves
        occupiedLocations = Dictionary<(int * int), ElfInfo>(elves |> Seq.map (fun elf -> KeyValuePair(elf.location, elf)))
    }

let makePlanForElf (strategies:LookStrategy[]) (state:State) (elfIndex:int) : State =
    let elf = state.elves.[elfIndex]
    let elfLocation = elf.location
    let possibleMoves = strategies
                            |> Array.choose (fun strategy ->
                                                if (strategy.offsetsToCheck |> Array.exists (fun (rowOffset, colOffset) -> 
                                                                                            let row = fst elfLocation + rowOffset
                                                                                            let col = snd elfLocation + colOffset
                                                                                            state.occupiedLocations.ContainsKey((row, col))
                                                                                        )
                                                    ) then
                                                    None
                                                else
                                                    Some strategy.offsetToMove
                                            )
    if (possibleMoves.Length > 0)
    then
        let (dr, dc) = possibleMoves[0]
        let (r, c) = elf.location
        let (row, col) = (r + dr, c + dc)
        elf.proposedLocation <- Some (row, col)
    else
        elf.proposedLocation <- None
    
    state

let moveElves (state:State) : State =
    state

let rec playRounds (state:State) =
    if (state.currentRound = state.roundsToPlay) then
        state
    else
        printfn "Starting round %d" state.currentRound

        (* First half of round; decide were qeach elf wants to move *)
        let strategyCount = state.lookStrategies.Length

        let orderedStrategies =
            seq { 0 .. strategyCount - 1 }
                |> Seq.map (fun i -> state.lookStrategies[(i + state.currentRound) % strategyCount])
                |> Array.ofSeq
     
        let planningState =
            seq { 0 .. state.elfCount - 1}
                |> Seq.fold (fun s i ->
                                let state = makePlanForElf orderedStrategies s i   
                                state
                            ) state

        (* Second half, the elves move if they are the only one with that plan *)

        let moveState = moveElves planningState

        playRounds { moveState with currentRound = moveState.currentRound + 1}


let solve =
    let lines = Common.getSampleDataAsArray 2022 23
    // let lines = Common.getChallengeDataAsArray 2022 23
    printAllLines lines
    printfn ""

    let occupied = parseLines lines
    printfn "%A" occupied
    let state = buildState occupied
    // printfn "\nState:\n %A" state
    let finalState = state
                        |> playRounds
    ()
