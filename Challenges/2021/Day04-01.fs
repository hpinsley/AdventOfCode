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

let makeBingoCard (cardLines:string[]) : BingoCard =
    let board = Array2D.create 5 5 { bingoNumber = 0; matched = false }
    
    {
        vCard = board
    }

let getBingoNumbers (line:string) : int[] =
    line.Split(',') |> Array.map int

let getBingoCards (lines:string[]) : BingoCard[] =
    let rec getCard (recs:string[]) (priors:BingoCard list) : BingoCard list =
        if (recs.Length > 0 && recs[0].Length = 0)
        then
            let cardLines = lines[1..5]
            let bingoCard = makeBingoCard cardLines
            let remaining = lines[6..]
            getCard remaining (bingoCard :: priors)
        else
            []

    getCard lines [] |> Array.ofList

let solve =
    let lines = Common.getSampleDataAsArray 2021 4
    // let lines = Common.getChallengeDataAsArray 2021 4
    printAllLines lines
    let bingoNumbers = getBingoNumbers lines[0]
    let bingoCards = getBingoCards lines[1..]

    printfn "Bingo numbers: %A" bingoNumbers
    printfn "Bingo cards:\n%A" bingoCards
    printfn "There are %d bingo cards" bingoCards.Length
    ()