module Year2022Day23_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let parseLines (lines:string[]) : (int * int) list =
    let parseLine (row:int) (line:string) : (int * int) seq =
       let occupied = line
                        |> Seq.mapi (fun col c -> match c with
                                                    | '#' -> Some (row, col)
                                                    | '.' -> None
                                                    | _ -> failwith "Invalid character"
                                    )
                        |> Seq.choose id
                        
       occupied
    let allRows = lines
                    |> Seq.mapi parseLine
                    |> Seq.concat
                    |> Seq.toList
    allRows

let solve =
    let lines = Common.getSampleDataAsArray 2022 23
    // let lines = Common.getChallengeDataAsArray 2022 23
    printAllLines lines
    printfn ""

    let occupied = parseLines lines
    printfn "%A" occupied
    ()
