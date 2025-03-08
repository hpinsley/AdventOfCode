module Year2022Day05_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions

type Instruction = {
    CratesToMove: int;
    FromCrate: int;
    ToCrate: int
}

let getCrateLetters inputLine =
    let matches = Regex.Matches(inputLine, "[\[ ]([ A-Z])[\] ] ?";);
    matches |> Seq.map (fun oneMatch -> oneMatch.Groups.[1].Value) |> List.ofSeq

let solve =

    // let lines = Common.getSampleDataAsArray 2022 05
    let lines = Common.getChallengeDataAsArray 2022 05
    // printfn "%A" lines

    // Break the input file into the top half data and the bottom half data looking for a blank line for that purpose
    let (reversedBlocks, _, reversedMoves) = lines 
                                                |> Seq.fold (fun (blocks, haveSeenBlank, moves) line -> 
                                                                    if (haveSeenBlank) then (blocks, haveSeenBlank, line :: moves)
                                                                    else if (line.Trim().Length = 0) then (blocks, true, moves)
                                                                    else (line :: blocks, haveSeenBlank, moves)
                                                            ) ([], false, [])
    // Fix up the order of the blocks (top half) and the moves (bottom half)
    let blocks = reversedBlocks |> List.tail |> List.rev
    let moves = reversedMoves |> List.rev

    //for b in blocks do
    //    printfn "%s" b

    let crates = blocks |> List.map getCrateLetters
    let numbered = crates
                    |> List.map (fun cols -> cols |> List.mapi (fun i col -> (i, col.[0])))
                    |> List.concat
                    |> List.filter (fun t -> (snd t) <> ' ')

    //printfn "%A" numbered
    let crateCount = (numbered |> List.map (fun t -> fst t) |> List.max) + 1
    printfn "There are %d crates" crateCount

    //Put the letters in an array of stacks (lists of letters)
    let state = Array.create crateCount []
    let stacks = numbered 
                    |> List.fold (fun (s:char list[]) (index, letter) -> (s[index] <- letter :: s[index]; s)) state
                    |> Array.map (fun crateList -> List.rev crateList)

    printfn "%A" stacks

    //printfn ("\nMoves:\n")

    //for m in moves do
    //    printfn "%s" m

    let moveRegex = "move (\d+) from (\d+) to (\d+)"

    let instructions =
            moves
                |> List.map (fun s -> (
                                        let m = Regex.Match(s, moveRegex); 
                                        {   CratesToMove = int m.Groups[1].Value;
                                            FromCrate = int m.Groups[2].Value;
                                            ToCrate = int m.Groups[3].Value
                                        }
                                       ))
    printfn "%A" instructions

    // Now we iterate through the instructions
    for instruction in instructions do
        let fromList = stacks[instruction.FromCrate - 1]
        let toMove = instruction.CratesToMove

        let (moving, remaining) = List.splitAt toMove fromList
        printfn "Moving %d crates from %d to %d: (%A, %A)" toMove instruction.FromCrate instruction.ToCrate remaining moving

        stacks[instruction.FromCrate - 1] <- remaining
        stacks[instruction.ToCrate - 1] <- List.append moving stacks[instruction.ToCrate - 1]
        //for b in moving do
        //    stacks[instruction.ToCrate - 1] <- b :: stacks[instruction.ToCrate - 1]

    let top = stacks |> Array.map List.head |> System.String
    printfn "%A" top





