module Year2022Day17_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Rock =
    {
        height: int;
        width: int;
        occupies: (int * int) Set;
    }

let RockTemplates = [|
                        { height = 1; width = 4; 
                            occupies = Set.ofList 
                                        [   
                                            (0,0);  (1,0);  (2,0);  (3,0)
                                        ]
                        }

                        { height = 3; width = 3; occupies = Set.ofList
                                        [
                                                    (1,0);
                                            (0,1);  (1,1);  (2,1);
                                                    (1,2)
                                        ]  
                        }
                    |]

let printRockTemplate (rock:Rock) : unit =
    let grid = Array2D.init rock.width rock.height (fun x y -> if rock.oc) 

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

    printfn "We have %d rock templates" RockTemplates.Length
    ()
