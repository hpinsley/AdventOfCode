module Year2018Day12

open System
open System.IO
open Common
open System.Text.RegularExpressions

// The sequence of bools are bits for constructing an Int by shifting
let boolsToInt (bools:bool seq) : int =
    let rec build boolList prior =
        match boolList with
            | alive :: rest ->
                let shifted = prior <<< 1
                let v = shifted + (if alive then 1 else 0)
                build rest v
            | [] ->
                prior

    build (List.ofSeq bools) 0

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

let buildInitialState (line:string) : bool list =
    line.Replace("initial state: ", "")
        |> Seq.toList
        |> List.map ((=) '#')

// We need to extend the state left by two in case a new plant can emerge.  Also right
// by two.  We also have to shift the currentPlantIndex
let extendState (vstate:bool array) (currentPlant:int) =
    let extended = Array.concat
                    [|
                        [|false; false|];
                        vstate;
                        [|false; false|]
                    |]
    (extended, currentPlant + 2)

let getStateAtIndexes (stateArray: bool array) (indexesToCheck: int array) =
    let maxIndex = stateArray.Length - 1
    indexesToCheck
        |> Array.map (fun index -> if index >= 0 && index <= maxIndex then stateArray.[index] else false)

let generate (rules:bool[]) (vstate:bool array) (currentPlant:int) =
    let (extendedArray, newCurrentPlantIndex) = extendState vstate currentPlant
    printfn "Extended %A" extendedArray

    let x =
        extendedArray
            |> Array.mapi (fun index _ -> [|index-2;index-1;index;index+1;index+2|])
            |> Array.map (getStateAtIndexes extendedArray)
            |> Array.map boolsToInt     // Find matching rule
            |> Array.map (fun ruleIndex -> rules.[ruleIndex])

    printfn "%A" x

let solvePartOne (rules:bool array) (initialState: bool list) =
    printfn "Starting part 1"
    let vstate = Array.ofList initialState
    let initialCount = vstate.Length
    printfn "Initially, there are %d pots" initialCount
    printfn "%A" vstate

    let result = generate rules vstate 0
    ()

let solvePartTwo () =
    printfn "Starting part 2"
    ()

let solve() =
    //let testdata = Common.getChallengeDataAsArray 2018 12
    let testdata = Common.getSampleDataAsArray 2018 12
    dump "data" testdata

    let rules = buildRules testdata.[2..]
    let intialState = buildInitialState testdata.[0]

    printfn "Rules:\n%A\n" rules
    printfn "Initial state: %A" intialState

    solvePartOne rules intialState

    //solvePartTwo()
    ()
