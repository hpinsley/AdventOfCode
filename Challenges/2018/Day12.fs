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
    let rules = testdata.[2..]
                    |> Array.map (fun line -> line.Split(" => "))
                    |> Array.map (fun [|p;r|] -> (p,r))
                    |> Array.map ruleToIndexAndBool
                    |> Array.fold (fun a r -> a) (Array.init 32)

    printfn "Rules:\n%A\n" rules
    solvePartOne()
    //solvePartTwo()
    ()
