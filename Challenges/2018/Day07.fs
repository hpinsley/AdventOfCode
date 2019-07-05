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

let rec orderSteps (dependencies: (char * char list) list) : char list =
    // To "execute a step, we have to add it to our output and also remove"
    // the step as a dependency of any successor in the dependencies list.
    // To determine the next step to execute, we look for items that have
    // no dependencies

    let removeNextToExecute (next:char) (dependencies: (char * char list) list): (char * char list) list =
        dependencies
            |> List.map (fun (s, preds) ->
                            preds
                                |> List.filter (fun pred -> pred <> next)
                                |> tuple2 s)
            |> List.filter (fun (s, _) -> s <> next)

    let eligible =
        dependencies
            |> List.filter (fun (s, predecessors) -> predecessors.Length = 0)
            |> List.map (fun (s, _) -> s)
            |> List.sort

    match eligible with
        | [] ->
            []
        | nextToExecute :: _ ->
            let newDependencyDb = removeNextToExecute nextToExecute dependencies
            //printfn "Executing step %c with %A" nextToExecute newDependencyDb
            nextToExecute :: (orderSteps newDependencyDb)

let solve =
    let testdata = Common.getChallengeDataAsArray 2018 7
    //let testdata = Common.getSampleDataAsArray 2018 7
    dump "data" testdata
    let pairs = testdata
                    |> List.ofArray
                    |> List.map (fun line -> (line.[5], line.[36]))
                    |> dump "pairs"

    let emptyDepenencyList =
        pairs
            |> List.map (fun (a,b) -> [a; b])
            |> List.concat
            |> List.distinct
            |> List.map (fun c -> (c, ([]: char list)))

    let dependencies = pairs
                        |> List.fold buildDependencies emptyDepenencyList

    dump "Dependencies" dependencies

    let stepOrder = orderSteps dependencies
                        |> Array.ofList
                        |> String
                        
    dump "Step order:" stepOrder

    ()
