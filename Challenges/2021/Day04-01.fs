module Year2021Day4_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type BingoCell =
    {
        bingoNumber: int
        mutable matched: bool
    }

type BingoCard =
    {
        vCard: BingoCell[,]
    }

let printCard (card:BingoCard) : unit =
    printfn ""
    for r in seq { 0 .. 4 } do
        printfn ""
        for c in seq { 0 .. 4 } do
            printf "%s: %2d" (if (card.vCard[r,c].matched) then "M" else " ") card.vCard[r,c].bingoNumber

let makeBingoCard (cardLines:string[]) : BingoCard =
    let values = cardLines |> Array.map (fun line -> line.Split(' ') |> Array.filter (fun s -> s.Length > 0) |> Array.map int)
    let board = Array2D.init 5 5 (fun r c -> { bingoNumber = values[r][c]; matched = false })
        
    {
        vCard = board
    }

let getBingoNumbers (line:string) : int[] =
    line.Split(',') |> Array.map int

let getBingoCards (lines:string[]) : BingoCard[] =
    let rec getCard (recs:string[]) (priors:BingoCard list) : BingoCard list =
        if (recs.Length > 0 && recs[0].Length = 0)
        then
            let cardLines = recs[1..5]
            let bingoCard = makeBingoCard cardLines
            let remaining = recs[6..]
            getCard remaining (bingoCard :: priors)
        else
            priors

    getCard lines [] |> List.rev |> Array.ofList

let solve =
    let lines = Common.getSampleDataAsArray 2021 4
    // let lines = Common.getChallengeDataAsArray 2021 4
    printAllLines lines
    let bingoNumbers = getBingoNumbers lines[0]
    let bingoCards = getBingoCards lines[1..]

    printfn "Bingo numbers: %A" bingoNumbers
    printfn "There are %d bingo cards" bingoCards.Length
    bingoCards |> Array.iter printCard
    ()