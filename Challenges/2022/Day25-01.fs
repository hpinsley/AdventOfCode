module Year2022Day25_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

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
    let base5 = convertBase10ToBaseN n 5
    let digits = base5 |> List.ofSeq |> List.map (string >> parseInt) |> List.map int64 |>List.rev
    let acc = (0L, [])
    let result = digits |>
                    List.fold (
                        fun (carry, prior) d ->
                            let newTotal = carry + d
                            let x = match newTotal with
                                    | 0L -> (0L, '0' :: prior)
                                    | 1L -> (0L, '1' :: prior)
                                    | 2L -> (0L, '2' :: prior)
                                    | 3L -> (1L, '=' :: prior)
                                    | 4L -> (1L, '-' :: prior)
                                    | 5L -> (1L, '0' :: prior)
                                    | _ -> failwith "Invalid digit"

                            x
                        ) acc

    let withoutFinalCarray = snd result |> List.ofSeq |> List.map string |> String.concat ""
    let finalCarry = fst result
    if (finalCarry = 0L)
    then
        withoutFinalCarray
    else
        string finalCarry + withoutFinalCarray

let testDecimalToSnafu() : unit =
    while (true) do
        printf("Enter a decimal number: ")
        let input = Console.ReadLine()
        let number = int input
        let converted = decimalToSnafu number
        printfn "%d decimal is %s snafu" number converted

let solve =
    // let lines = Common.getSampleDataAsArray 2022 25
    let lines = Common.getChallengeDataAsArray 2022 25
    printAllLines lines
    let total = lines
                |> Array.fold (fun total line -> total + snafuToDecimal line) 0L
    printfn "Total is %d" total

    let answer = decimalToSnafu total
    printfn "Part 1: %s" answer
    ()