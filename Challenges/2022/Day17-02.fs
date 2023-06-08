module Year2022Day17_Part2

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type RockTemplate =
    {
        templateId: int;
        rows: int;
        cols: int;
        occupies: (int * int) Set;      // Let's use rows and cols not x and y
    }

type Rock =
    {
        rows: int;
        cols: int;
        mutable occupies: (int * int) Set;      // Let's use rows and cols not x and y
    }

type Cave =
    {
        maxHeight: int;
        rocks: Rock list
    }

let caveWidth = 7

let (rockTemplates:RockTemplate[]) = [|
                            { templateId = 0; rows = 1; cols = 4; 
                                occupies = Set.ofList 
                                            [   
                                                (0,0);  (0,1);  (0,2);  (0,3)
                                            ]
                            }

                            { templateId = 1; rows = 3; cols = 3; occupies = Set.ofList
                                            [
                                                        (2,1);
                                                (1,0);  (1,1);  (1,2);
                                                        (0,1);
                                            ]  
                            }                        
 
                            { templateId = 2; rows = 3; cols = 3; occupies = Set.ofList
                                            [
                                                                (2,2);
                                                                (1,2);
                                                (0,0);  (0,1);  (0,2);
                                            ]  
                            }                        
                            { templateId = 3; rows = 4; cols = 1; 
                                occupies = Set.ofList 
                                            [   
                                                (3,0);
                                                (2,0);
                                                (1,0);
                                                (0,0);
                                            ]
                            }

                            { templateId = 4; rows = 2; cols = 2; occupies = Set.ofList
                                            [
                                                (1,0);  (1,1);
                                                (0,0);  (0,1);
                                            ]  
                            }                        
                        |]

let (heightWhenNewFloorDetected:int option array) = Array.create rockTemplates.Length None

let getTopRowColumns (cave:Cave) : Set<int> =
    let topColumns = cave.rocks
                        |> Seq.map (fun rock -> rock.occupies)
                        |> Seq.concat
                        |> Seq.filter (fun (r,c) -> r = cave.maxHeight)
                        |> Seq.map (fun (r,c) -> c)
                        |> Set.ofSeq

    topColumns

// Get the "depth" of each column.  The depth is
// max height across all columns minus the height of the column
let getColumnDepths (cave:Cave) =
    let allRockPositions = cave.rocks
                            |> Seq.map (fun rock -> rock.occupies)
                            |> Seq.concat

    let colSequence = seq { 1 .. caveWidth }

    let floor = colSequence |> Seq.map (fun c -> (0, c))

    let allOccupants = Seq.append allRockPositions floor

    let depthByColumn = colSequence
                                |> Seq.map (
                                                fun c ->
                                                    let colHeight =
                                                        allOccupants
                                                            |> Seq.filter (fun (row,col) -> col = c)
                                                            |> Seq.map (fun (row, col) -> cave.maxHeight - row)
                                                            |> Seq.min
                                                    (c, colHeight)
                                            )
                                |> Map.ofSeq


    depthByColumn

//let getRepeatState (totalRockCount:int64) (rockTemplateId:int) (cave:Cave) : RepeatDetectState =
//    {
//        totalRockCount = totalRockCount;
//        lastRockTemplateId = rockTemplateId;
//        topLayers = cave.rocks
//    }
let printRockTemplate (rock:RockTemplate) : unit =
    let grid = Array2D.init rock.rows rock.cols (fun row col -> if Set.contains (rock.rows - 1 - row,col) rock.occupies then '#' else '.') 
    printGrid grid id

let printCave (cave:Cave) : unit =
    let grid = Array2D.create (cave.maxHeight + 1) 9 false
    for rock in cave.rocks do
        for (r,c) in rock.occupies do
            let flippedRow = cave.maxHeight - r
            grid[flippedRow,c] <- true
    printGrid grid (fun b -> if b then '#' else '.')

let getWindDirection (line:string) : int[] =
    line
        |> Seq.map (fun c -> match c with
                                |'<' -> -1
                                |'>' -> 1
                                | _ -> failwith "unknown direction")
        |> Array.ofSeq

let canMoveDown (cave:Cave) (rock:Rock) : bool =
    let minRow = rock.occupies |> Seq.map fst |> Seq.min
    if (minRow = 0)
    then
        false
    else
        cave.rocks
            |> Seq.exists (fun caveRock -> Set.intersect caveRock.occupies rock.occupies
                                            |> Set.isEmpty
                                            |> not)
            |> not
                        
let canBlowSideways (cave:Cave) (rock:Rock) : bool =
    let cols = rock.occupies |> Seq.map snd
    let minCol = Seq.min cols
    let maxCol = Seq.max cols

    if (minCol <= 0 || maxCol > caveWidth)
    then
        false
    else
        cave.rocks
            |> Seq.exists (fun caveRock -> Set.intersect caveRock.occupies rock.occupies
                                            |> Set.isEmpty
                                            |> not)
            |> not

let maxHeight (rock:Rock) : int =
    rock.occupies
        |> Seq.map (fun (row, col) -> row)
        |> Seq.max

let solvePart2 (maxRocksToFall:int64) (initialCave:Cave) (windEnumerator:IEnumerator<int>) (rockTemplateEnumerator:IEnumerator<RockTemplate>) : Cave =
    let mutable (cave:Cave) = initialCave

    for r in seq { 0L .. maxRocksToFall - 1L} do
        printfn "Dropping rock %d" r
        
        rockTemplateEnumerator.MoveNext() |> ignore
        let template = rockTemplateEnumerator.Current

        //printRockTemplate template
        //printfn ""
        //printfn "Cave before it falls\n"
        //printCave cave

        let startingRow = cave.maxHeight + 4
        let startingCol = 3

        let mutable (rock:Rock) = {
            rows = template.rows
            cols = template.cols
            occupies = template.occupies
                        |> Seq.map (fun (row,col) -> (row + startingRow, col + startingCol))
                        |> Set.ofSeq
        }

        //printRockTemplate template
        //printfn "\n"

        let mutable rockCameToRest = false
        while (not rockCameToRest) do
            // First blow sideways.  Get the wind, and shift right or left if possible
            windEnumerator.MoveNext() |> ignore
            let wind = windEnumerator.Current
            let mutable movedRock = { rock with occupies = rock.occupies
                                                    |> Seq.map (fun (row, col) ->
                                                                    row, col + wind)
                                                    |> Set.ofSeq
                            }
            //printfn "Rock: %A, Moved Rock: %A" rock movedRock
            if (canBlowSideways cave movedRock)
            then
                rock <- movedRock

            // Now try to move down
            movedRock <- { rock with occupies = rock.occupies
                                                    |> Seq.map (fun (row, col) ->
                                                                    row - 1, col)
                                                    |> Set.ofSeq
                            }
            if (canMoveDown cave movedRock)
            then
                rock <- movedRock
            else
                rockCameToRest <- true
                cave <- { cave with rocks = rock :: cave.rocks
                                    maxHeight = max cave.maxHeight (maxHeight rock)
                        }
        printfn "\nAfter rock %d cave is:" r
        printCave cave
        printfn ""


        let colDepths = getColumnDepths cave
        printfn "^-- Coldepths are %A" colDepths

        let totalRockCount = r + 1L
        let topRow = getTopRowColumns cave
        let colCount = Seq.length topRow
        //printfn ""
        //printCave cave
        printfn "Rocks count: %d. Top row has filled %d columns" totalRockCount colCount
        if (colCount = caveWidth)
        then
            // Floor detected
            if (Option.isSome heightWhenNewFloorDetected[template.templateId])
            then
                printfn "Found repeat for template id %d" template.templateId
            else
                heightWhenNewFloorDetected[template.templateId] <- Some cave.maxHeight

    cave


let solve =
    // let lines = Common.getSampleDataAsArray 2022 17
    let lines = Common.getChallengeDataAsArray 2022 17

    // printAllLines lines
    printfn "There are %d lines in the input and the first one is %d chars" lines.Length lines[0].Length

    let result = findCycle lines[0]

    let windDirections = getWindDirection lines[0]
    printfn "%A (of length %d)" windDirections windDirections.Length

    let windStream = Seq.initInfinite (fun i -> windDirections[i % windDirections.Length])
    let rockTemplateStream = Seq.initInfinite (fun i -> rockTemplates[i % rockTemplates.Length])
    let windEnumerator = windStream.GetEnumerator()
    let rockTemplateEnumerator = rockTemplateStream.GetEnumerator()

    //while rockTemplateEnumerator.MoveNext() do
    //    let mutable rock = rockEnumerator.Current
    //    printRockTemplate rock
    //    printfn ""

    printfn "We have %d rock templates" rockTemplates.Length

    let(cave:Cave) = {
        maxHeight = 0;
        rocks = []
    }

    printfn "Cave: %A" cave

    let maxRocksToFall = 20000L
    let requestedRocksToFall = 1_000_000_000_000L
    let repeatFactor = 2671L
    let remainder = requestedRocksToFall % repeatFactor
    let factor = requestedRocksToFall / repeatFactor

    printfn "Remainder is %d" remainder
    printfn "Factor is %d" factor
    
    let finalCave = solvePart2 maxRocksToFall cave windEnumerator rockTemplateEnumerator
    
    //printfn "\nFinal cave:\n"
    //printCave finalCave
    printfn "\nAnswer: %d" finalCave.maxHeight
    ()
