module Year2021Day8_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Digit =
    {
        number: int;
        letters: char Set;
        letterCount: int
    }

type LengthGroup =
    {
        letterCount: int;
        digitCount: int;
        digits: Digit[]
    }

type Puzzle =
    {
        observedSignals: Set<char>[]
        outputValues: Set<char>[]
    }

type SignalToSegment =
    {
        signal: char
        segment: char option
    }

type State =
    {
        digits: Digit[];
        digitsByLength: LengthGroup[];
        distinctDigits: Digit[];
        distinctLengths: Set<int>;
        puzzleSignals: Set<char>[];
        decodedSignals: Dictionary<char,char>;
        puzzleOutputValues: Set<char>[];
    }

let stringToCharSet (s:string) : char Set =
    s |> Set.ofSeq

let parseInputLine (line:string) : Puzzle =
    let groups = line.Split('|')
                    |> Array.map (fun s -> s.Trim())
    let pattern = {
                    observedSignals = groups[0].Split(' ') |> Array.map stringToCharSet
                    outputValues = groups[1].Split(' ') |> Array.map stringToCharSet
                  }
    pattern

let parseInput (lines:string[]) : Puzzle[] =
    lines |> Array.map parseInputLine

let buildDigitMap () : Digit[] =
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
                                        letters = stringToCharSet s;
                                        letterCount = s.Length
                                    }
                              )

(*
    What if we did this in multiple passes with a flag (changed) and iterate until there is nothing to do.
    If the problems can be solved deterministically, then at that point, we should have all signal lines
    decoded.

    What things should we track in each pass?
        - The input signal "patterns" for which we know the corresponding digit being represented.
          Knowing that an input pattern corresponds to a digit doesn't necessarily imply that we know the mapping
          from each and every source signal to digit "segment".
          We will call this "knownSignalPatterns"

        - Known mappings of any individual signal to a segment.
          We will call this "knownSignals" -- a Dictionary<char, char>

    What things can we do in each pass?

        - Set changes to False
        - For Each inputSignalPattern in the 10 inputSignalPatterns
            Run each of these rules against the inputSignalPattern
                - Get the length of the inputSignalPattern?
        - Check the changes flag.
*)

type KnownSignalPattern =
    {
        inputSignals: char Set
        outputSegments: char Set
        digit: int
    }

let buildState (digits:Digit[]) (puzzles:Puzzle[]): State =
    let digitsByLength = digits |> Array.groupBy (fun d -> d.letterCount) |> Array.map (fun (length,digits) -> { letterCount = length; digitCount = digits.Length; digits = digits; }) 
    let distinctDigits = digitsByLength |> Array.filter (fun dbl -> dbl.digitCount = 1) |> Array.map (fun dbl -> dbl.digits[0])
    let distinctLengths = distinctDigits |> Array.map (fun d -> d.letterCount) |> Set.ofArray

    let puzzle = puzzles[0]

    let state =
            {
                digits = digits;
                digitsByLength = digitsByLength;
                distinctDigits = distinctDigits;
                distinctLengths = distinctLengths;
                puzzleSignals = puzzle.observedSignals;
                decodedSignals = new Dictionary<char,char>();
                puzzleOutputValues = puzzle.outputValues;
            }
    state

let dumpState (state:State) : unit =
    printfn "\nState:\n%A\n" state
    printfn "Digit Set Analysis\n"

    printfn "Distinct digits"
    for dd in state.distinctDigits do
        printfn "%A" dd

    printfn ""

    for i in seq { 0 .. state.digits.Length - 1 } do
        for j in seq { i + 1 .. state.digits.Length - 1 } do
            let d1 = state.digits[i]
            let d2 = state.digits[j]
            if (Set.isProperSubset d1.letters d2.letters)
            then
                let d2Diff = Set.difference d2.letters d1.letters
                if (d2Diff.Count <= 1)
                then
                    printfn "%d (%A) is a subset of %d (%A) differing by %d segments (%A)" d1.number d1.letters d2.number d2.letters d2Diff.Count d2Diff
            elif (Set.isProperSubset d2.letters d1.letters)
            then
                let d1Diff = Set.difference d1.letters d2.letters
                if (d1Diff.Count <= 1)
                then
                    printfn "%d (%A) is a subset of %d (%A) differing by %d segments (%A)" d2.number d2.letters d1.number d1.letters d1Diff.Count d1Diff

    printfn ""

    for dbl in state.digitsByLength do
        printfn "\n%d signals" dbl.letterCount
        for d in dbl.digits do
            printfn "\t(%d) - %A" d.number d.letters

    printfn "\nObserved:\n"

    for sigSet in state.puzzleSignals do
        printfn "%A (%d letters) => ?" sigSet sigSet.Count 

let solvePartOne (state:State) (puzzles:Puzzle[]) =
    let allOutputValues = puzzles |> Array.map (fun p -> p.outputValues) |> Array.concat
    let easyOutputValues = allOutputValues |> Array.filter (fun s -> Set.contains s.Count state.distinctLengths )
    printfn "There are %d easy ones" easyOutputValues.Length

let solve =
    let lines = Common.getSampleDataAsArray 2021 8
    // let lines = Common.getChallengeDataAsArray 2021 8
    let puzzles = parseInput lines

    //printAllLines lines
    let digits = buildDigitMap()
    let state = buildState digits puzzles

    solvePartOne state puzzles

    //dumpState state

    ()
