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
    location: (int * int)
    proposedLocation: (int * int)
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

type State = 
    {
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
                    |> Seq.mapi (fun id (row, col) -> { id = id; location = (row, col); proposedLocation = (-1, -1) })
                    |> Seq.toArray
    
    {
        elfCount = elves.Length
        currentRound = 0
        roundsToPlay = ROUNDS_TO_PLAY
        elves = elves
        occupiedLocations = Dictionary<(int * int), ElfInfo>(elves |> Seq.map (fun elf -> KeyValuePair(elf.location, elf)))
    }

let solve =
    let lines = Common.getSampleDataAsArray 2022 23
    // let lines = Common.getChallengeDataAsArray 2022 23
    printAllLines lines
    printfn ""

    let occupied = parseLines lines
    printfn "%A" occupied
    let state = buildState occupied
    printfn "\nState:\n %A" state
    ()
