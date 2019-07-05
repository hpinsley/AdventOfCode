module Year2018Day07

open System
open System.IO
open Common


let rec buildDependencies (dependencies: (char * char list) list) (newPair: (char * char)): (char * char list) list =
    let (newpred, newsucc) = newPair
    match dependencies with
        | [] ->
            [newsucc, [newpred]]
        | (s, plist) :: remainingDependencies when (s = newsucc) ->
            (s, newpred :: plist) :: remainingDependencies
        | firstDependency :: remainingDependencies ->
            firstDependency :: buildDependencies remainingDependencies newPair

let solve =
    //let testdata = Common.getChallengeDataAsArray 2018 7
    let testdata = Common.getSampleDataAsArray 2018 7
    dump "data" testdata
    let pairs = testdata
                    |> List.ofArray
                    |> List.map (fun line -> (line.[5], line.[36]))
                    |> dump "pairs"

    let dependencies = pairs
                        |> List.fold buildDependencies []

    dump "Dependencies" dependencies
    ()
