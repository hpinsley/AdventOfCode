module Year2018Day15

open System
open System.IO
open Common
open System.Text.RegularExpressions

type PlayerType = Elf | Goblin

type Player = {
    playerType: PlayerType;
    power: int;
    hitPoints: int;
    row: int;
    col: int;
}

type Cell =
      Empty
    | Occupied of Player
    | Wall

type State =
    {
        grid: Cell[,];
        rounds: int;
        waitingToPlay: Player list;
        activePlayer: Player option;
    }

let getPlayerLetter (playerType: PlayerType) : char =
    match playerType with
        | Elf -> 'E'
        | Goblin -> 'G'

let playerToString (player:Player) : string =
    sprintf "%c at [%d,%d] with hit points = %d"
                            (getPlayerLetter player.playerType) player.row player.col player.hitPoints

let playersToString (players: Player list) : string =
    players
        |> List.map playerToString
        |> String.concat ";"

let mapCellToChar (c:Cell) : char =
    match c with
        | Empty -> '.'
        | Wall -> '#'
        | Occupied unit ->
            match unit.playerType with
                | Elf -> 'E'
                | Goblin -> 'G'

let printGrid grid =
    Common.printGrid grid mapCellToChar

let getIndexList (game:Cell[,]) : (int * int) list =
    let rows = Array2D.length1 game
    let cols = Array2D.length2 game
    [for r in 0..rows-1 do for c in 0..cols - 1 do yield (r,c)]

let getPlayersInOrder (game:Cell[,]) =
    getIndexList game
        |> List.choose (fun (r,c) ->
                            match game.[r,c] with
                                | Occupied player -> Some player
                                | _ -> None)
        |> List.sortBy (fun player -> (player.row, player.col))

let noFoes players =
    (List.forall (fun p -> p.playerType = Elf) players)
    ||
    (List.forall (fun p -> p.playerType = Goblin) players)

let printState (state:State) : unit =
    printfn "\nGame state\n"
    printGrid state.grid
    printfn "%s" (playersToString (getPlayersInOrder state.grid))

let buildInitialState (lines:string[]) : State =

    let rows = lines.Length
    let cols = lines |> Array.map (fun line -> line.Length) |> Array.max
    let grid = Array2D.init rows cols (fun row col ->
                                        match lines.[row].[col] with
                                            | '.' -> Empty
                                            | '#' -> Wall
                                            | 'E' -> Occupied {
                                                            playerType = Elf;
                                                            hitPoints = 200;
                                                            power = 3;
                                                            row = row;
                                                            col = col;
                                                        }
                                            | 'G' -> Occupied {
                                                            playerType = Goblin;
                                                            hitPoints = 200;
                                                            power = 3;
                                                            row = row;
                                                            col = col;
                                                        }
                                            | _ -> failwith "Unexpected character"
                                        )

    {
        grid = grid;
        rounds = 0;
        waitingToPlay = [];
        activePlayer = None
    }

let getEnemies (state:State) (player:Player) : (Player list) =
    getPlayersInOrder state.grid
        |> List.filter (fun p -> p.playerType <> player.playerType)

let computeShortestPathToEnemy (state:State) (player:Player) (enemy:Player) =
    printfn "Computing shortest path from player %s to enemy %s" (playerToString player) (playerToString enemy)
    "xx"

let matchPlayerAgainst (state:State) (player:Player) (enemies:Player list) : State =
    printfn "Enemies of %s are: %s" (playerToString player) (playersToString enemies)
    let pathsToEnemies =
        enemies
            |> List.map (computeShortestPathToEnemy state player)
    state

let startPlayerTurn (state:State) (player:Player) : State =
    printfn "Starting player %s in round %d" (playerToString player) state.rounds
    let enemies = getEnemies state player
    match enemies with
        | [] -> state
        | _ -> matchPlayerAgainst state player enemies

let rec selectNextPlayer (state:State) : State =
    match state.waitingToPlay with
        | [] -> state
        | player :: rest ->
            let newState = { state with waitingToPlay = rest }
            let result = startPlayerTurn newState player
            selectNextPlayer result

let rec startRound (state:State) : State =
    let playersToMove = getPlayersInOrder state.grid
    if (noFoes playersToMove)
    then
        state
    else
        let nextRound = state.rounds + 1
        if (nextRound > 3)
        then
            state
        else
            let newState = { state with rounds = nextRound; waitingToPlay = playersToMove }
            let result = selectNextPlayer newState
            startRound result

let solve() =
    printfn "Day 15"
    //let testdata = Common.getChallengeDataAsArray 2018 15
    let testdata = Common.getSampleDataAsArray 2018 15
    //dump "data" testdata
    let state = buildInitialState testdata
    printState state

    let final = startRound state
    printState final
    ()
