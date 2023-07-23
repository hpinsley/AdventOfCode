module Year2022Day25_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let testBaseArithmetic() : unit =
    while (true) do
    printf("Enter a base 10 number: ")
    let input = Console.ReadLine()
    let number = int input
    let converted = convertBase10ToBaseN number 5
    printfn "%d base 10 is %d base 5" number converted

let testBaseToDecimalArithmetic() : unit =
    while (true) do
        printf("Enter a base 5 number: ")
        let input = Console.ReadLine()
        let number = int input
        let converted = convertBaseNToBase10 number 5
        printfn "%d base 5 is %d base 10" number converted

let snafuDigitValue (c:char) : int64 =
    match c with
        | '0' -> 0L
        | '1' -> 1L
        | '2' -> 2L
        | '-' -> -1L
        | '=' -> -2L
        | _ -> failwith "Invalid digit"

let snafuToDecimal (s:string) : int64 =
    let snafuDigits = List.ofSeq s |> List.rev
    let (grandTotal, _) = snafuDigits
                            |> List.fold (fun (total, placeValue) v -> 
                                (total + placeValue * (snafuDigitValue v), placeValue * 5L)
                                         ) 
                               (0L, 1L)
    grandTotal

let decimalToSnafu (n:int64) : string =
    let base5 = convertBase10ToBaseN n 5 |> string
    let digits = base5 |> List.ofSeq |> List.map int64 |> List.rev
    let acc = (0, [])
    ""

let solve =
    let lines = Common.getSampleDataAsArray 2022 25
    // let lines = Common.getChallengeDataAsArray 2022 25
    printAllLines lines
    let total = lines
                |> Array.fold (fun total line -> total + snafuToDecimal line) 0L
    printfn "Total is %d" total

    testBaseToDecimalArithmetic()

    ()