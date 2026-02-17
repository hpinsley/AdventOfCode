module Year2025Day7_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

type Cell =
    | Beam of (int * int)
    | Splitter of (int * int)
    | Empty of (int * int)
    
let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2025 7
    // let lines: string array = Common.getChallengeDataAsArray 2025 7

    let rows = lines.Length
    let cols = lines[0].Length
    let startCol = lines[0].IndexOf("S")

    let vData = Array2D.init rows cols (fun r c -> lines[r][c])
    let bData = vData |> Array2D.mapi  (fun row col c ->
                                                        match c with
                                                            | 'S' | '|' -> Beam (row, col)
                                                            | '^' -> Splitter (row, col)
                                                            | '.' -> Empty (row, col)
                                                            | _ -> raise (Exception("Unexpected character"))

                                                     )
    Common.printGrid vData id
    Common.printGrid bData (fun c ->
                                                match c with
                                                    | Beam _ -> '|'
                                                    | Splitter _ -> '^'
                                                    | Empty _ -> ' '
                                            )

    printfn "\n%d rows and %d cols.  The start col index is %d\n" rows cols startCol


    ()