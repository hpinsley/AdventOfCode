module Year2022Day17_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Rock =
    {
        rows: int;
        cols: int;
        occupies: (int * int) Set;      // Let's use rows and cols not x and y
    }

let RockTemplates = [|
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

let printRockTemplate (rock:Rock) : unit =
    let grid = Array2D.init rock.rows rock.cols (fun row col -> if Set.contains (row,col) rock.occupies then '#' else '.') 
    printGrid grid id

let getWindDirection (line:string) : int[] =
    line
        |> Seq.map (fun c -> match c with
                                |'<' -> -1
                                |'>' -> 1
                                | _ -> failwith "unknown direction")
        |> Array.ofSeq


let solve =
    // let lines = Common.getSampleDataAsArray 2022 17
    let lines = Common.getChallengeDataAsArray 2022 17

    printAllLines lines
    printfn "There are %d lines in the input and the first one is %d chars" lines.Length lines[0].Length

    let windDirections = getWindDirection lines[0]
    printfn "%A (of length %d)" windDirections windDirections.Length

    let windStream = Seq.initInfinite (fun i -> windDirections[i % windDirections.Length])
    let rockStream = Seq.initInfinite (fun i -> RockTemplates[i % RockTemplates.Length])
    let windEnumerator = windStream.GetEnumerator()
    let rockEnumerator = rockStream.GetEnumerator()

    while rockEnumerator.MoveNext() do
        let mutable rock = rockEnumerator.Current
        printRockTemplate rock
        printfn ""

    printfn "We have %d rock templates" RockTemplates.Length
    for rock in RockTemplates do
        printfn ""
        printRockTemplate rock
    ()
