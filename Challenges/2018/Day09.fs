module Year2018Day09

open System
open System.IO
open Common

type GameState = {
    numberOfPlayers: int;
    lastMarbleValue: int;
    playerScores: int list;
    currentMarbleIndex: int;
    board: int list;
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
        board = [0];
        currentPlayer = 0;
    }

let playStandard (game:GameState) (marbleValue:int): GameState =
    let clockPlusOne = clockwiseFrom game.currentMarbleIndex 1 game.board.Length
    let clockPlusTwo = clockwiseFrom game.currentMarbleIndex 2 game.board.Length
    let splitIndex = if (clockPlusTwo > clockPlusOne) then clockPlusTwo else clockPlusOne + 1
    let (l1, l2) = List.splitAt splitIndex game.board
    let newBoard = List.concat [l1; [marbleValue]; l2]
    let newCurrentIndex = splitIndex % newBoard.Length
    let nextPlayer = (game.currentPlayer + 1) % game.numberOfPlayers

    { game with board = newBoard; currentMarbleIndex = newCurrentIndex; currentPlayer = nextPlayer }

let play23 (game:GameState) (marbleValue:int): GameState =
    let counterBy7 = counterClockwiseFrom game.currentMarbleIndex 7 game.board.Length
    let moveScore = marbleValue + game.board.[counterBy7];
    let newScores = List.mapi (fun i score -> if i = game.currentPlayer then score + moveScore else score) game.playerScores
    let (l, r) = List.splitAt counterBy7 game.board
    let newBoard = match r with
                    | toDrop :: rest ->
                        List.concat [l; rest]
                    | [] ->
                        match l with
                            | toDrop :: rest ->
                                rest
                            | _ ->
                                l
    let newCurrentIndex = counterBy7 % newBoard.Length
    let nextPlayer = (game.currentPlayer + 1) % game.numberOfPlayers

    { game with playerScores = newScores; board = newBoard; currentMarbleIndex = newCurrentIndex; currentPlayer = nextPlayer }

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
    let initialBoard = buildInitialBoard 428 72061
    printfn "Part one"

    //dump "Initial board" initialBoard

    let finalBoard = [1..initialBoard.lastMarbleValue]
                        |> List.fold playMarble initialBoard

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
