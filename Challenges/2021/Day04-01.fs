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

let markNumberOnOneCard (card:BingoCard) (n:int) : bool =
    let mutable (matchingCell:Option<int * int>) = None

    card.vCard |> Array2D.iteri (fun r c bingoCell -> 
                                    if (bingoCell.bingoNumber = n) 
                                    then
                                        bingoCell.matched <- true
                                        matchingCell <- Some (r, c)
                                )

    match matchingCell with
        | None -> false
        | Some (matchingRow, matchingCol) ->
            let rowWins = seq {0 .. 4 } 
                             |> Seq.map (fun c -> card.vCard[matchingRow, c])
                             |> Seq.exists (fun vCard -> not vCard.matched)
                             |> not
            let colWins = seq {0 .. 4 } 
                             |> Seq.map (fun r -> card.vCard[r, matchingCol])
                             |> Seq.exists (fun vCard -> not vCard.matched)
                             |> not
            rowWins || colWins

let markNumberOnAllCards (cards:BingoCard[]) (calledNumber:int) : BingoCard option =
    let mutable (winningCard:BingoCard option) = None
    cards |> Array.iter (fun card -> 
                            let cardWins = markNumberOnOneCard card calledNumber
                            if (cardWins)
                            then
                                winningCard <- Some card
                          )
    winningCard

let playBingo (cards:BingoCard[]) (numbers:int[]) : (Option<BingoCard> * int) =
    let mutable (winningCard:BingoCard option) = None
    let mutable winningNumber = 0
    numbers |> Array.iter (fun n -> 
                                match winningCard with
                                    | None -> 
                                        winningCard <- markNumberOnAllCards cards n
                                        if (winningCard |> Option.isSome)
                                        then
                                            winningNumber <- n
                                        ()
                                    | Some card ->
                                        //Ignore the remaining numbers
                                        ()
                          )
    
    (winningCard, winningNumber)

let computeScore (winningBoard:BingoCard option) (winningNumber:int) =
    let mutable unmarkedCount = 0
    match winningBoard with
        | None -> 0
        | Some card ->
            card.vCard
                |> Array2D.iter (fun cell -> 
                                    if (not cell.matched) 
                                        then
                                            unmarkedCount <- unmarkedCount + cell.bingoNumber
                                )
            unmarkedCount * winningNumber



let solve =
    // let lines = Common.getSampleDataAsArray 2021 4
    let lines = Common.getChallengeDataAsArray 2021 4
    // printAllLines lines
    
    let bingoNumbers = getBingoNumbers lines[0]
    let bingoCards = getBingoCards lines[1..]

    printfn "Bingo numbers: %A" bingoNumbers
    printfn "There are %d bingo cards" bingoCards.Length
    //bingoCards |> Array.iter printCard

    printfn "\nPlaying..."

    let (winningBoard, winningNumber) = playBingo bingoCards bingoNumbers
    let score = computeScore winningBoard winningNumber

    printfn "\nScore: %d" score
    ()