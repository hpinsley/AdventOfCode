module Year2024Day4_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Direction =
    | North
    | South
    | East
    | West
    | NorthEast
    | NorthWest
    | SouthEast
    | SouthWest

type Movement =
    | Move of direction:Direction * row:int * col:int

type Fragment =
    {
        fragment: string
        tail: int * int
        movement: Movement
    }

let movements = [
        Move(North, -1, 0)
        Move(South, 1, 0)
        Move(East, 0, -1)
        Move(West, 0, 1)
        Move(NorthEast, -1, 1)
        Move(NorthWest, -1, -1)
        Move(SouthEast, 1, 1)
        Move(SouthWest, 1, -1)
    ]

type MoveResult = (int * int) option

let move (from:int * int) (how:Movement) (rows:int) (cols:int): MoveResult =
    let deltaRow, deltaCol = match how with
                                | Move (_, r, c) -> r,c
    let row = fst from + deltaRow
    let col = snd from + deltaCol

    if row >= 0 && row < rows && col >=0 && col < cols
    then
        Some (row, col)
    else
        None

let extendFragments (targetLetter:char) (fragments:Fragment list) (grid:char[,]): Fragment list =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid

    let good = fragments |> List.filter (fun fragment -> 
                                let moveResult = move fragment.tail fragment.movement rows cols
                                match moveResult with
                                    | Some (r2, c2) ->
                                        if grid.[r2,c2] = targetLetter
                                            then true
                                        else
                                            false
                                    | None -> false
                            )

    good |> List.map (fun fragment -> 
                            let (r2,c2) = move fragment.tail fragment.movement rows cols |> Option.get
                            { 
                                fragment = fragment.fragment + targetLetter.ToString();
                                tail = (r2,c2);
                                movement = fragment.movement
                            }
                    )

let part1 (grid:char[,]) : int =

    let target = "XMAS"
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid

    //First find letter positions

    let indexPairs = seq {  for r in {0..rows-1} do
                            for c in {0..cols-1} do
                            yield (r,c) } |> Seq.toList

    let firstLetter = target[0]
    let anchorCells = indexPairs |> List.filter (fun tpl -> grid.[fst tpl, snd tpl] = firstLetter)

    // From the anchors, construct the initial list of fragments
    let secondLetter = target[1]

    let fragments = anchorCells |> List.fold (fun agg (r,c) -> 
                                            let validMoves = movements 
                                                                |> List.filter (fun movement -> 
                                                                                    let moveResult = move (r,c) movement rows cols
                                                                                    match moveResult with
                                                                                        | Some (r2, c2) ->
                                                                                            if grid.[r2,c2] = secondLetter
                                                                                                then true
                                                                                            else
                                                                                                false
                                                                                        | None -> false
                                                                                )
                                            let fragments = validMoves |>
                                                                List.map (fun validMove -> 
                                                                            let (r2,c2) = move (r,c) validMove rows cols |> Option.get
                                                                            { fragment = target[0..1];
                                                                                tail = (r2,c2);
                                                                                movement = validMove
                                                                            }
                                                                         )
                                            List.append agg fragments) []:Fragment list

    // Now we have a list of two letter fragments and we know which direction we have to follow

    let remainingLetters = target[2..]

    let finalList = remainingLetters |> Seq.fold (fun fragments targetLetter -> 
                                                        extendFragments targetLetter fragments grid
                                                  )
                                                  fragments

    finalList.Length
    


let solve =
    // let lines = Common.getSampleDataAsArray 2024 4
    let lines = Common.getChallengeDataAsArray 2024 4

    //printfn "%A" lines

    let rows = lines.Length
    let cols = lines[0].Length

    let grid = Array2D.init rows cols (fun i j -> lines[i][j])
    //printGrid grid id

    let part1Result = part1 grid
    printfn "Part 1: There are %d instances of XMAS" part1Result

    ()