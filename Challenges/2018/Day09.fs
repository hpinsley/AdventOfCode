module Year2018Day09

open System
open System.IO
open Common

type GameState = {
    numberOfPlayers: int;
    lastMarbleValue: int;
    playerScores: int list;
    currentMarbleIndex: int;
    numberOfMarblesOnBoard: int;
    board: int [];
    currentPlayer: int
}

let clockwiseFrom index toMove total =
    (index + toMove) % total

let counterClockwiseFrom index toMove total =
    (index - toMove + total) % total

let buildInitialBoard numberOfPlayers lastMarbleValue =
    {
        numberOfPlayers = numberOfPlayers;
        lastMarbleValue = lastMarbleValue;
        playerScores = List.init numberOfPlayers (fun _ -> 0)
        currentMarbleIndex = 0;
        numberOfMarblesOnBoard = 1;
        board = Array.zeroCreate lastMarbleValue
        currentPlayer = 0;
    }

let playStandard (game:GameState) (marbleValue:int): GameState =
    let clockPlusOne = clockwiseFrom game.currentMarbleIndex 1 game.numberOfMarblesOnBoard
    let clockPlusTwo = clockwiseFrom game.currentMarbleIndex 2 game.numberOfMarblesOnBoard

    let newIndex = if clockPlusTwo > clockPlusOne then clockPlusTwo else clockPlusOne + 1
    // We want to place a marble at newIndex.  Shift the array right

    let itemsToCopy = game.numberOfMarblesOnBoard - newIndex
    let _ = Array.Copy (game.board, newIndex, game.board, newIndex + 1, itemsToCopy)

    game.board.[newIndex] <- marbleValue

    let numberOfMarblesOnBoard = game.numberOfMarblesOnBoard + 1
    let nextPlayer = (game.currentPlayer + 1) % game.numberOfPlayers

    { game with currentMarbleIndex = newIndex; numberOfMarblesOnBoard = numberOfMarblesOnBoard; currentPlayer = nextPlayer }

let play23 (game:GameState) (marbleValue:int): GameState =
    let removeIndex = counterClockwiseFrom game.currentMarbleIndex 7 game.numberOfMarblesOnBoard

    let removeScore = game.board.[removeIndex]
    let moveScore = marbleValue + removeScore

    // printfn "Scored %d + %d = %d (from index removed at %d)" marbleValue removeScore moveScore removeIndex

    let newScores = List.mapi (fun i score -> if i = game.currentPlayer then score + moveScore else score) game.playerScores

    // We want to remove the marble at newIndex.  Shift the array left

    let itemsToCopy = game.numberOfMarblesOnBoard - removeIndex - 1

    Array.Copy (game.board, removeIndex + 1, game.board, removeIndex, itemsToCopy)

    let numberOfMarblesOnBoard = game.numberOfMarblesOnBoard - 1

    let nextPlayer = (game.currentPlayer + 1) % game.numberOfPlayers

    { game with playerScores = newScores; numberOfMarblesOnBoard = numberOfMarblesOnBoard; currentMarbleIndex = removeIndex; currentPlayer = nextPlayer }

let playMarble (game:GameState) (marbleValue:int): GameState =
    //printfn "Playing marble %d with board\n%A" marbleValue game
    if marbleValue % 23 = 0
    then
        play23 game marbleValue
    else
        playStandard game marbleValue

let solvePartOne  =
    //let initialBoard = buildInitialBoard 9 25
    //let initialBoard = buildInitialBoard 10 1618
    //let initialBoard = buildInitialBoard 13 7999
    //let initialBoard = buildInitialBoard 17 1104
    //let initialBoard = buildInitialBoard 21 6111
    //let initialBoard = buildInitialBoard 30 5807
    //let initialBoard = buildInitialBoard 428 72061
    let initialBoard = buildInitialBoard 428 7206100
    printfn "Part one"

    //dump "Initial board" initialBoard

    let finalBoard = [1..initialBoard.lastMarbleValue]
                        |> List.fold playMarble initialBoard

    // dump "player scores" finalBoard.playerScores

    printfn "Max score is %d" <| List.max finalBoard.playerScores
    //dump "Final board" finalBoard

    ()

let solvePartTwo  =
    printfn "part two"

let solve =
    //let testdata = Common.getChallengeData 2018 9
    //let testdata = Common.getSampleData 2018 9
    // dump "data" testdata

    solvePartOne
    solvePartTwo
    ()
