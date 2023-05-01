module Year2022Day07_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions

type Instruction =
    | DIR of string
    | CD of string
    | LS
    | File of int * string

// ParseRegex parses a regular expression and returns a list of the strings that match each group in
// the regular expression.
// List.tail is called to eliminate the first element in the list, which is the full matched expression,
// since only the matches for each group are wanted.
let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let getInstruction (line:string): Instruction =
    match line with
        | ParseRegex "^\$ cd (.*)" [folder] -> CD folder
        | ParseRegex "^\$ ls" [] -> LS
        | ParseRegex "^dir (.*)" [folder] -> DIR folder
        | ParseRegex "^([0-9]+) (.*)" [size; filename] -> File (int size, filename)

let solve =

    // let lines = Common.getSampleDataAsArray 2022 07
    let lines = Common.getChallengeDataAsArray 2022 07
    printfn "%A" lines

    let instructions = lines |> Array.map getInstruction

    //let paired = Array.zip lines instructions
    //printfn "%A" paired

    for instruction in instructions do
        printfn "%A" instruction
