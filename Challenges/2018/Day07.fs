module Year2018Day07

open System
open System.IO
open Common

type Task = {
    name: char;
    startTime: int
    endTime: int
}

type Worker =
    | Idle
    | WorkingOn of Task

let rec buildDependencies (dependencies: (char * char list) list) (newPair: (char * char)): (char * char list) list =
    let (newpred, newsucc) = newPair
    match dependencies with
        | [] ->
            [newsucc, [newpred]]
        | (s, plist) :: remainingDependencies when (s = newsucc) ->
            (s, newpred :: plist) :: remainingDependencies
        | firstDependency :: remainingDependencies ->
            firstDependency :: buildDependencies remainingDependencies newPair

let getNextToExecute (dependencies: (char * char list) list) =
    dependencies
        |> List.filter (fun (s, predecessors) -> predecessors.Length = 0)
        |> List.map (fun (s, _) -> s)
        |> List.sort
        |> List.tryHead

let startTask (next:char) (dependencies: (char * char list) list): (char * char list) list =
    dependencies
        |> List.filter (fun (s, _) -> s <> next)

let endTask (next:char) (dependencies: (char * char list) list): (char * char list) list =
    dependencies
        |> List.map (fun (s, preds) ->
                        preds
                            |> List.filter (fun pred -> pred <> next)
                            |> tuple2 s)

let executeImmediately (next:char) (dependencies: (char * char list) list): (char * char list) list =
    dependencies
        |> startTask next
        |> endTask next

let rec orderSteps (dependencies: (char * char list) list) : char list =
    // To "execute a step, we have to add it to our output and also remove"
    // the step as a dependency of any successor in the dependencies list.
    // To determine the next step to execute, we look for items that have
    // no dependencies

    match getNextToExecute dependencies with
        | None ->
            []
        | Some nextToExecute ->
            let newDependencyDb = executeImmediately nextToExecute dependencies
            nextToExecute :: (orderSteps newDependencyDb)

let solvePartOne (dependencies: (char * char list) list) =
    let stepOrder = orderSteps dependencies
                        |> Array.ofList
                        |> String

    dump "Part (1): Step order:" stepOrder
    stepOrder

let solvePartTwo (workerCount: int) (durationFunc: char -> int) (dependencies: (char * char list) list) =
    printfn "\nPart II has %d workders and %d steps" workerCount dependencies.Length
    let workers = List.replicate workerCount Idle
    dump "workers:" workers

    let stepOrder = orderSteps dependencies
                        |> Array.ofList
                        |> String
    stepOrder

let solve =
    //let testdata = Common.getChallengeDataAsArray 2018 7
    let testdata = Common.getSampleDataAsArray 2018 7
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

    solvePartOne dependencies |> ignore

    let durationFunc c = (int c - 65 + 1) + 0
    //let durationFunc c = (int c - 65 + 1) + 60

    printfn "Duration of %c is %d" 'A' (durationFunc 'A')
    solvePartTwo 2 durationFunc dependencies |> ignore
    ()
