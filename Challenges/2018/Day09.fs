module Year2018Day09

open System
open System.IO
open Common

type Circle<'a> =
    {
        pred: 'a list;
        succ: 'a list;
        value: 'a;
    }

let insertBefore value circle =
    {
        circle with
            succ = circle.value :: circle.succ
            value = value
    }

let insertAfter value circle =
    {
        circle with
            pred = circle.value :: circle.pred
            value = value
    }

let rec removeCurrent circle : Circle<'a> * 'a =
        match circle.succ with
            | v :: rest ->
                ({ circle with value = v; succ = rest}, circle.value)
            | [] ->
                match circle.pred with
                    | [] ->
                        raise (Exception "Can't remove from empty")
                    | predecessors ->
                        removeCurrent { circle with succ = List.rev predecessors }

let rec rotateClockwise c =
    match c.pred with
        | p :: rest ->
            { c with value = p; pred = rest; succ = c.value :: c.succ}
        | [] ->
            match c.succ with
                | [] ->
                    c
                | successors ->
                    rotateClockwise {
                        c with pred = List.rev successors; succ = []
                    }

let rec rotateCounterClockwise c =
    match c.succ with
        | s :: rest ->
            { c with value = s; succ = rest; pred = c.value :: c.pred}
        | [] ->
            match c.pred with
                | [] ->
                    c
                | predecessors ->
                    rotateCounterClockwise {
                        c with succ = List.rev predecessors; pred = []
                    }

type GameState = {
    numberOfPlayers: int;
    lastMarbleValue: int64;
    playerScores: int64 [];
    board: Circle<int64>;
    currentPlayer: int
}

let buildInitialBoard numberOfPlayers lastMarbleValue =
    {
        numberOfPlayers = numberOfPlayers;
        lastMarbleValue = lastMarbleValue;
        playerScores = Array.init numberOfPlayers (fun _ -> 0L)
        board = { value = 0L; pred = []; succ = [] }
        currentPlayer = 0;
    }

let playStandard (game:GameState) (marbleValue:int64): GameState =

    // We want to place a marble at newIndex.  Shift the array right

    let nextPlayer = (game.currentPlayer + 1) % game.numberOfPlayers
    let nextBoard = game.board
                    |> rotateCounterClockwise
                    |> insertAfter marbleValue

    { game with currentPlayer = nextPlayer; board = nextBoard }

let play23 (game:GameState) (marbleValue:int64): GameState =
    let rotated = [1..7]
                    |> List.fold (fun c i -> rotateClockwise c) game.board

    let (newBoard, removeScore) = removeCurrent rotated
    let moveScore = marbleValue + removeScore

    // printfn "Scored %d + %d = %d (from index removed at %d)" marbleValue removeScore moveScore removeIndex

    game.playerScores.[game.currentPlayer] <- game.playerScores.[game.currentPlayer] + moveScore
    // We want to remove the marble at newIndex.  Shift the array left

    let nextPlayer = (game.currentPlayer + 1) % game.numberOfPlayers

    { game with board = newBoard; currentPlayer = nextPlayer }

let playMarble (game:GameState) (marbleValue:int64): GameState =
    //printfn "Playing marble %d with board\n%A" marbleValue game
    if marbleValue % 23L = 0L
    then
        play23 game marbleValue
    else
        playStandard game marbleValue

let solvePartOne  =
    //let initialBoard = buildInitialBoard 9 25L
    //let initialBoard = buildInitialBoard 10 1618L
    //let initialBoard = buildInitialBoard 13 7999L
    //let initialBoard = buildInitialBoard 17 1104L
    //let initialBoard = buildInitialBoard 21 6111L
    //let initialBoard = buildInitialBoard 30 5807L
    //let initialBoard = buildInitialBoard 428 72061L
    let initialBoard = buildInitialBoard 428 7206100L
    printfn "Part one"

    //dump "Initial board" initialBoard

    let finalBoard = [1L..initialBoard.lastMarbleValue]
                         |> List.fold playMarble initialBoard

    // dump "player scores" finalBoard.playerScores

    printfn "Max score is %d" <| Array.max finalBoard.playerScores
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
