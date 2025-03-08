module Year2024Day8_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

type Location =
    {
        row: int
        col: int
    }

// Given two antenna's compute the distance between them and use that to compute two antinode locations
let computeAntinodeLocationsPart1 (rows:int) (cols:int) (antenna1:Location) (antenna2:Location) : Location list =

    let isValidLocation loc = loc.row >= 0 && loc.row < rows && loc.col >= 0 && loc.col < cols
    // Compute vector from a1 to a2
    let rowDelta = antenna2.row - antenna1.row
    let colDelta = antenna2.col - antenna1.col

    // One antinode is a1 - v and the other is a2 + v

    let location1 = { row = antenna1.row - rowDelta; col = antenna1.col - colDelta }
    let location2 = { row = antenna2.row + rowDelta; col = antenna2.col + colDelta }
    
    let unvalidatedAntinodes = [location1; location2]
    unvalidatedAntinodes |> List.filter isValidLocation

// Given two antenna's compute the distance between them and use that to compute as many antinodes
// that fit in the grid
let computeAntinodeLocationsPart2 (rows:int) (cols:int) (antenna1:Location) (antenna2:Location) : Location list =
   
    let isValidLocation loc = loc.row >= 0 && loc.row < rows && loc.col >= 0 && loc.col < cols
    
    let rec walkLocations (loc1:Location) (loc2:Location) (rowDelta:int) (colDelta:int) : Location list =
        let (loc1Valid, loc2Valid) = (isValidLocation loc1, isValidLocation loc2)

        match (loc1Valid, loc2Valid) with
            | (false, false) -> []
            | (true, false) ->  
                let n1 = { row = loc1.row - rowDelta; col = loc1.col - colDelta }
                loc1 :: walkLocations n1 loc2 rowDelta colDelta
            | (false, true) ->
                let n2 = { row = loc2.row + rowDelta; col = loc2.col + colDelta }
                loc2 :: walkLocations loc1 n2 rowDelta colDelta
            | (true, true) ->
                let n1 = { row = loc1.row - rowDelta; col = loc1.col - colDelta }
                let n2 = { row = loc2.row + rowDelta; col = loc2.col + colDelta }
                loc1 :: loc2 :: walkLocations n1 n2 rowDelta colDelta

    // Compute vector from a1 to a2
    let rowDelta = antenna2.row - antenna1.row
    let colDelta = antenna2.col - antenna1.col

    // Antinodes is a1 - N*v and the other is a2 + Mv for n>=1 and m>=1 until we are off the grid
        
    let allLocations = walkLocations antenna1 antenna2 rowDelta colDelta
    allLocations

let determineAntiNodes (rows:int) (cols:int) (antennaLocations: (char * (Location[])) list) : Location list =
    
    let frequencyPairs = antennaLocations
                            |> List.map (fun tpl -> List.ofSeq (allCombinations (snd tpl)))
    // At this point, we don't care about the antenna frequencies as we have determined all pairs of 
    // the same frequency

    let allPairs = List.concat frequencyPairs
    let antinodes = allPairs
                        //|> List.map (fun locTpl -> computeAntinodeLocationsPart1 rows cols (fst locTpl) (snd locTpl))
                        |> List.map (fun locTpl -> computeAntinodeLocationsPart2 rows cols (fst locTpl) (snd locTpl))
                        |> List.concat                    

    let uniqueAntinodeLocations = antinodes |> Set.ofList |> List.ofSeq
    uniqueAntinodeLocations

let solve =
    let stopWatch = Stopwatch.StartNew()

    // let lines = Common.getSampleDataAsArray 2024 8
    let lines = Common.getChallengeDataAsArray 2024 8

    let rows = lines.Length
    let cols = lines[0].Length

    let grid = Array2D.init rows cols (fun i j -> lines[i][j])
    // printGrid grid id

    let possibleLocations = allRowColPairs rows cols
                            |> Seq.map (fun (r,c) -> { row = r; col = c })
                            |> List.ofSeq

    let antennas = possibleLocations |> List.fold (fun state loc ->
                                                    let chr = grid[loc.row, loc.col]
                                                    if chr <> '.'
                                                    then
                                                        (chr, loc) :: state
                                                    else
                                                        state
                                                  ) 
                                                  []


    let groupedAntennas = antennas
                                    |> List.groupBy fst
                                    |> List.map (fun kvp -> (fst kvp, List.map snd (snd kvp)))
                                    |> List.map (fun kvp -> (fst kvp, Array.ofList (snd kvp)))

    let antiNodes = determineAntiNodes rows cols groupedAntennas

    let part1Time = stopWatch.ElapsedMilliseconds
 
    printfn "Part1: There are %d unique antinode locations" antiNodes.Length
    stopWatch.Restart()

    // Part 2


    let part2Time = stopWatch.ElapsedMilliseconds;
    printfn "Timings.  Part 1: %dms, Part 2: %dms" part1Time part2Time

    ()