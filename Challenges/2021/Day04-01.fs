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
        mutable hasWon: bool;
        mutable winningNumber: int;
        mutable callIndex: int
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
        hasWon = false
        winningNumber = 0
        callIndex = -1
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
                                card.hasWon <- true
                                winningCard <- Some card
                          )
    winningCard

let markNumberOnAllCardsPart2 (cards:BingoCard[]) (callIndex:int) (calledNumber:int) : unit =
    
    cards |> Array.iter (fun card -> 
                            if (not card.hasWon)
                            then
                                let cardWins = markNumberOnOneCard card calledNumber
                                if (cardWins)
                                then
                                    card.hasWon <- true
                                    card.callIndex <- callIndex
                                    card.winningNumber <- calledNumber
                          )
    ()

let playBingoToWin (cards:BingoCard[]) (numbers:int[]) : (Option<BingoCard> * int) =
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

let playBingoToLose (cards:BingoCard[]) (numbers:int[]) : (Option<BingoCard> * int) =

    numbers |> Array.iteri (fun callIndex n -> markNumberOnAllCardsPart2 cards callIndex n)
    
    let winningCardCount = cards |> Array.filter (fun c -> c.hasWon) |> Array.length
    if (winningCardCount = cards.Length)
    then
        // Find the last to win
        let lastToWin = cards |> Array.maxBy (fun card -> card.callIndex)
        (Some lastToWin, lastToWin.winningNumber)
    else
        (None, 0)

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


let solvePart1 (lines:string[]) =
    let bingoNumbers = getBingoNumbers lines[0]
    let bingoCards = getBingoCards lines[1..]

    printfn "Bingo numbers: %A" bingoNumbers
    printfn "There are %d bingo cards" bingoCards.Length
    //bingoCards |> Array.iter printCard

    printfn "\nPlaying..."

    let (winningBoard, winningNumber) = playBingoToWin bingoCards bingoNumbers
    let score = computeScore winningBoard winningNumber

    printfn "\n(1) Score: %d" score
    ()

let solvePart2 (lines:string[]) =
    let bingoNumbers = getBingoNumbers lines[0]
    let bingoCards = getBingoCards lines[1..]

    printfn "Bingo numbers: %A" bingoNumbers
    printfn "There are %d bingo cards" bingoCards.Length
    //bingoCards |> Array.iter printCard

    printfn "\nPlaying..."

    let (winningBoard, winningNumber) = playBingoToLose bingoCards bingoNumbers
    let score = computeScore winningBoard winningNumber

    printfn "\n(2) Score: %d" score
    ()

let solve =
    // let lines = Common.getSampleDataAsArray 2021 4
    let lines = Common.getChallengeDataAsArray 2021 4
    // printAllLines lines
    solvePart1 lines
    solvePart2 lines
    ()