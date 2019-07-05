module Year2018Day07

open System
open System.IO
open Common


let buildDependencies (successors: (char * char list) list) (newPair: (char * char)): (char * char list) list =
    let rec addPair (dependencies: (char * char list) list) (newPair: (char * char)): (char * char list) list =
        let (newpred, newsucc) = newPair
        match dependencies with
            | [] -> [newsucc, [newpred]]
            | (s, plist) as firstDependency :: remainingDependencies ->
                if (s = newsucc) then
                    (s, newpred :: plist) :: remainingDependencies
                else
                    firstDependency :: addPair remainingDependencies newPair

    addPair successors newPair

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
