module Year2022Day05_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions

let solve =

    // let lines = Common.getSampleDataAsArray 2022 05
    let lines = Common.getChallengeDataAsArray 2022 05
    // printfn "%A" lines

    let (reversedBlocks, _, reversedMoves) = lines 
                                                |> Seq.fold (fun (blocks, haveSeenBlank, moves) line -> 
                                                                    if (haveSeenBlank) then (blocks, haveSeenBlank, line :: moves)
                                                                    else if (line.Trim().Length = 0) then (blocks, true, moves)
                                                                    else (line :: blocks, haveSeenBlank, moves)
                                                            ) ([], false, [])
    let blocks = reversedBlocks |> List.rev
    let moves = reversedMoves |> List.rev

    for b in blocks do
        printfn "%s" b

    printfn ("\nMoves:\n")

    for m in moves do
        printfn "%s" m




