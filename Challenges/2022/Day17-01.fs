module Year2022Day17_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type RockTemplate =
    {
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

let (rockTemplates:RockTemplate[]) = [|
                            { rows = 1; cols = 4; 
                                occupies = Set.ofList 
                                            [   
                                                (0,0);  (0,1);  (0,2);  (0,3)
                                            ]
                            }

                            { rows = 3; cols = 3; occupies = Set.ofList
                                            [
                                                        (0,1);
                                                (1,0);  (1,1);  (1,2);
                                                        (2,1)
                                            ]  
                            }                        
 
                            { rows = 3; cols = 3; occupies = Set.ofList
                                            [
                                                                (0,2);
                                                                (1,2);
                                                (2,0);  (2,1);  (2,2);
                                            ]  
                            }                        
                            { rows = 4; cols = 1; 
                                occupies = Set.ofList 
                                            [   
                                                (0,0);
                                                (1,0);
                                                (2,0);
                                                (3,0);
                                            ]
                            }

                            { rows = 2; cols = 2; occupies = Set.ofList
                                            [
                                                (0,0);  (0,1);
                                                (1,0);  (1,1);
                                            ]  
                            }                        
                        |]

let printRockTemplate (rock:RockTemplate) : unit =
    let grid = Array2D.init rock.rows rock.cols (fun row col -> if Set.contains (row,col) rock.occupies then '#' else '.') 
    printGrid grid id

let getWindDirection (line:string) : int[] =
    line
        |> Seq.map (fun c -> match c with
                                |'<' -> -1
                                |'>' -> 1
                                | _ -> failwith "unknown direction")
        |> Array.ofSeq

let solvePart1 (cave:Cave) (windEnumerator:IEnumerator<int>) (rockTemplateEnumerator:IEnumerator<RockTemplate>) : unit =
    let maxRocksToFall = 2
    for r in seq { 0 .. maxRocksToFall - 1} do
        printfn "Dropping rock %d" r
        rockTemplateEnumerator.MoveNext() |> ignore
        let template = rockTemplateEnumerator.Current
        let startingRow = cave.maxHeight + 4
        let startingCol = 3

        let (rock:Rock) = {
            rows = template.rows
            cols = template.cols
            occupies = template.occupies
                        |> Seq.map (fun (row,col) -> (row + startingRow, col + startingCol))
                        |> Set.ofSeq
        }
        printfn "Rock %d = %A" r rock
        ()


let solve =
    let lines = Common.getSampleDataAsArray 2022 17
    // let lines = Common.getChallengeDataAsArray 2022 17

    printAllLines lines
    printfn "There are %d lines in the input and the first one is %d chars" lines.Length lines[0].Length

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

    solvePart1 cave windEnumerator rockTemplateEnumerator
    ()
