﻿module Year2021Day8_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Digit =
    {
        number: int;
        segments: string;
        segCount: int;
    }

type LengthGroup =
    {
        segCount: int;
        digitCount: int;
        digits: Digit[]
    }

type Pattern =
    {
        observedSignals: string[]
        outputValues: string[]
    }

let parseInputLine (line:string) : Pattern =
    let groups = line.Split('|')
                    |> Array.map (fun s -> s.Trim())
    let pattern = {
                    observedSignals = groups[0].Split(' ');
                    outputValues = groups[1].Split(' ')
                  }
    pattern

let parseInput (lines:string[]) : Pattern[] =
    lines |> Array.map parseInputLine

let buildDigitMap() : Digit[] =
    let definitions = 
        [|
            "abcefg"    //0
            "cf"        //1
            "acdeg"     //2
            "acdfg"     //3
            "bcdf"      //4
            "abdfg"     //5
            "abdefg"    //6
            "acf"       //7
            "abcdefg"   //8
            "abcdfg"    //9
        |]

    definitions |> Array.mapi (
                                fun i s -> 
                                    {
                                        number = i;
                                        segments = s;
                                        segCount = s.Length
                                    }
                              )

    

let solve =
    // let lines = Common.getSampleDataAsArray 2021 8
    let lines = Common.getChallengeDataAsArray 2021 8

    //printAllLines lines
    let digits = buildDigitMap()
    let digitsByLength = digits 
                            |> Array.groupBy (fun d -> d.segCount)
                            |> Array.map (fun (length,digits) -> 
                                            { segCount = length;
                                              digitCount = digits.Length;
                                              digits = digits;
                                            })

    //printfn "%A" digitsByLength

    let distinctDigits = digitsByLength 
                                    |> Array.filter (fun dbl -> dbl.digitCount = 1)
                                    |> Array.map (fun dbl -> dbl.digits[0])

    let distinctLengths = distinctDigits 
                            |> Array.map (fun d -> d.segCount)
                            |> Set.ofArray

    //printfn "The following digits are distinct by length:\n"
    //printfn "%A" distinctDigits

    let input = parseInput lines
    //printfn "\nInput is:\n"
    //printfn "%A" input
    printfn "\nDistinct lengths are: %A" distinctLengths

    let allOutputValues = input 
                            |> Array.map (fun p -> p.outputValues)
                            |> Array.concat

    printfn "There are %d output values." allOutputValues.Length

    let easyOutputValues = allOutputValues
                            |> Array.filter (
                                                fun s ->
                                                    Set.contains s.Length distinctLengths
                                             )
    printfn "There are %d easy ones" easyOutputValues.Length

    let pattern = input[0]
    printfn "%A" pattern

    ()
