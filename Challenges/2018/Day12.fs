module Year2018Day12

open System
open System.IO
open Common
open System.Text.RegularExpressions

let patternToInt (pattern:string) : int =
    let rec build pattern prior =
        match pattern with
            | c :: rest ->
                let shifted = prior <<< 1
                let v = match c with
                            | '#' -> shifted + 1
                            | '.' -> shifted
                            | _ -> failwith (sprintf "Invalid char in pattter: [%A]" c)
                build rest v
            | [] ->
                prior

    build (Seq.toList pattern) 0

let ruleToIndexAndBool (pattern,indicator) =
    let alive = indicator = "#"
    (patternToInt pattern, alive)

let testPattern pattern =
    let result = patternToInt pattern
    printfn "Pattern %s = %d" pattern result

// Given an array of strings of the form:
// ..### => .
// Construct an index between 0..31 for the left hand side where the # are binary 1
// and the . is binary 0.  This is the index.  And the right hand side is true (#) or false
// (.)

let buildRules (encodedRules:string array) =
    let splitLines = encodedRules |> Array.map (fun line -> line.Split(" => "))

    let rules = (Array.map ((fun [|pattern;aliveIndicator|] -> (pattern,aliveIndicator)) >> ruleToIndexAndBool) splitLines)
                    |> Array.fold
                            (fun (rules:bool array) (arrayIndex:int, alive:bool) ->
                                rules.[arrayIndex] <- alive
                                rules
                             )
                            (Array.init 32 (fun _ -> false))
    rules

let solvePartOne () =
    printfn "Starting part 1"

    ()

let solvePartTwo () =
    printfn "Starting part 2"
    ()

let solve() =
    //let testdata = Common.getChallengeDataAsArray 2018 12
    let testdata = Common.getSampleDataAsArray 2018 12
    dump "data" testdata

    let rules = buildRules testdata.[2..]

    printfn "Rules:\n%A\n" rules
    solvePartOne()
    //solvePartTwo()
    ()
