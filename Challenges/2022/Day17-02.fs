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

type RepeatDetectState = {
    totalRockCount: int;
    lastRockTemplateId: int;
    windIndex: int;
    caveHeight: int;
    colDepths: Set<int * int>;
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

let repeatStatesByRockType: RepeatDetectState list[] = 
    Array.create rockTemplates.Length []

type RepeatSizes = {
            rockCountBeforeCycle: int;
            heightBeforeCycle: int;
            cycleLength: int;
            cycleHeight: int;
            firstRockTemplateIdToCycle: int;
            startOfFirstCycle: RepeatDetectState
    }

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
let getColumnDepths (cave:Cave) : (int * int) Set =
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
                                |> Set.ofSeq


    depthByColumn

let getRepeatState (windIndex:int) (totalRockCount:int) (rockTemplateId:int) (cave:Cave) : RepeatDetectState =
    {
        totalRockCount = totalRockCount
        lastRockTemplateId = rockTemplateId
        windIndex = windIndex
        colDepths = getColumnDepths cave
        caveHeight = cave.maxHeight
    }

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

let solvePart2 (maxRocksToFall:int) (initialCave:Cave) (windDirections:int[]) (rockTemplateEnumerator:IEnumerator<RockTemplate>) : (RepeatSizes option * Cave) =
    let mutable (cave:Cave) = initialCave

    let mutable r = 0
    let mutable windIndex = -1
    let mutable foundRepeatFactor = None

    while ((Option.isNone foundRepeatFactor) && r < maxRocksToFall) do
        printfn "Dropping rock %d with cave height = %d" r cave.maxHeight
        
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
            windIndex <- (windIndex + 1) % windDirections.Length
            let wind = windDirections[windIndex]
            //printfn "Wind: %d" wind
            let mutable movedRock = { rock with occupies = rock.occupies
                                                    |> Seq.map (fun (row, col) ->
                                                                    row, col + wind)
                                                    |> Set.ofSeq
                            }
            //printfn "Rock: %A, Moved Rock: %A" rock movedRock
            if (canBlowSideways cave movedRock)
            then
                //printfn "Rock %d MOVES horizontally %s" r (if wind = -1 then "Left" else "Right")
                rock <- movedRock
            else
                ()
                //printfn "Rock %d CANNOT move horizontally %s" r (if wind = -1 then "Left" else "Right")

            // Now try to move down
            movedRock <- { rock with occupies = rock.occupies
                                                    |> Seq.map (fun (row, col) ->
                                                                    row - 1, col)
                                                    |> Set.ofSeq
                            }
            if (canMoveDown cave movedRock)
            then
                //printfn "Rock %d moved down 1" r
                rock <- movedRock
            else
                rockCameToRest <- true
                cave <- { cave with rocks = rock :: cave.rocks
                                    maxHeight = max cave.maxHeight (maxHeight rock)
                        }
        // Rock has come to reset here
        //printfn ""
        //printCave cave
        //printfn ""

        let totalRockCount = r + 1
        let repeatState = getRepeatState windIndex totalRockCount template.templateId cave

        // We store our previous states in a an array (indexed by rock type) of state lists
        let previousStates = repeatStatesByRockType[template.templateId]
        // We have repeated when the 
        let matchingPrevStates =
                    previousStates
                        |> List.filter (fun s -> s.colDepths = repeatState.colDepths && 
                                                                s.windIndex = repeatState.windIndex)

        if (matchingPrevStates.Length > 0)
        then
            let tallestMatch = matchingPrevStates[0]
            //printfn ""
            //printCave cave

            printfn "Found repeat after rock %d for template id %d, wind index %d. Previous height %d. New height %d" 
                        cave.rocks.Length template.templateId windIndex tallestMatch.caveHeight repeatState.caveHeight

            foundRepeatFactor <- Some {
                                        startOfFirstCycle = tallestMatch;
                                        rockCountBeforeCycle = tallestMatch.totalRockCount;
                                        heightBeforeCycle = tallestMatch.caveHeight;
                                        cycleLength = repeatState.totalRockCount - tallestMatch.totalRockCount;
                                        cycleHeight = repeatState.caveHeight - tallestMatch.caveHeight
                                        firstRockTemplateIdToCycle = template.templateId
                                       }
            
        repeatStatesByRockType[template.templateId] <- repeatState :: repeatStatesByRockType[template.templateId]
        r <- r + 1

    // Finished with the rocks
    (foundRepeatFactor, cave)

let solve =
    // let lines = Common.getSampleDataAsArray 2022 17
    let lines = Common.getChallengeDataAsArray 2022 17

    // printAllLines lines
    printfn "There are %d lines in the input and the first one is %d chars" lines.Length lines[0].Length

    let result = findCycle lines[0]

    let windDirections = getWindDirection lines[0]
    printfn "%A (of length %d)" windDirections windDirections.Length

    let rockTemplateStream = Seq.initInfinite (fun i -> rockTemplates[i % rockTemplates.Length])
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

    let maxRocksToFall = 2022
    
    let requestedRocksToFall = 1_000_000_000_000L
    // let requestedRocksToFall = 2022L
    //let repeatFactor = 26L
    //let remainder = requestedRocksToFall % repeatFactor
    //let factor = requestedRocksToFall / repeatFactor

    //printfn "Remainder is %d" remainder
    //printfn "Factor is %d" factor
    
    let (repeatCountsOption, cave)  = solvePart2 maxRocksToFall cave windDirections rockTemplateEnumerator

    match repeatCountsOption with
        | Some repeatCounts ->
            let rocksAfterCyclesStart = requestedRocksToFall - int64(repeatCounts.rockCountBeforeCycle)
            let numCyles = rocksAfterCyclesStart / (int64(repeatCounts.cycleLength))
            let rocksToDropAfterLastCycle = int(rocksAfterCyclesStart % (int64(repeatCounts.cycleLength)))

            let resultRockCountToFind = repeatCounts.rockCountBeforeCycle + rocksToDropAfterLastCycle

            let x = repeatStatesByRockType
                        |> Array.map (fun l -> Array.ofList l)
                        |> Array.concat
                        |> Array.find (fun rs -> rs.totalRockCount = resultRockCountToFind)

            let heightAfterLastCycle = x.caveHeight - repeatCounts.startOfFirstCycle.caveHeight

            let totalHeight = int64(repeatCounts.heightBeforeCycle) + 
                              numCyles * int64(repeatCounts.cycleHeight) +
                              int64(heightAfterLastCycle)

            // let correctAnswer = 3068L
            let correctAnswer = 1514285714288L
            let difference = correctAnswer - totalHeight
            printfn "Our answer is %d (difference is %d)" totalHeight difference

            ()
        | None ->
            failwith "No repetition found"
    
    ()
