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

let isIdle worker = match worker with
                        | Idle -> true
                        | WorkingOn _ -> false

let isBusy = (not << isIdle)
let allIdle = not << (List.exists isBusy)

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

let getNextCompletedTask (workers: Worker list) : Task option =

    let tasks = workers |> List.choose (fun worker ->
                                            match worker with
                                                | Idle -> None
                                                | WorkingOn task -> Some task)

    if tasks.Length = 0
        then None
    else
        List.minBy (fun task -> task.endTime) tasks
            |> Some

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

let rec idleWorker (whoIsWorkingOnTask:char) (workers: Worker list) : Worker list =
    match workers with
        | [] -> []
        | worker :: rest ->
            match worker with
                | Idle ->
                    worker :: idleWorker whoIsWorkingOnTask rest
                | WorkingOn task when task.name = whoIsWorkingOnTask ->
                    Idle :: rest
                | _ ->
                    worker :: idleWorker whoIsWorkingOnTask rest

// Add a task to the worker list.  If there is an available worker return
// Some workers otherwise none if there are no available workers
let rec addTaskToWorkList (taskName: char) (currentTime: int) (durationFunc: char -> int) (workers: Worker list) : (Worker list) option =
    match workers with
        | [] -> None
        | worker :: others ->
            match worker with
                | Idle ->
                    let activeWorker = WorkingOn { name = taskName; startTime = currentTime; endTime = currentTime + durationFunc taskName }
                    Some (activeWorker :: others)
                | busyWorker ->
                    match addTaskToWorkList taskName currentTime durationFunc others with
                        | None ->
                            None
                        | Some revisedWorkerList ->
                            Some (busyWorker :: revisedWorkerList)

let rec step (workers: Worker list) (durationFunc: char -> int) (dependencies: (char * char list) list) (startTime:int) :int =
    match getNextToExecute dependencies with
        | None ->
            // There is nothing to do.  Let's see if some workers are working and can be complete
            match getNextCompletedTask workers with
                | None ->
                    startTime               //
                | Some completedTask ->
                    step
                        (idleWorker completedTask.name workers)
                        durationFunc
                        (endTask completedTask.name dependencies)
                        completedTask.endTime

        | Some taskName ->      // There is work to do
            // Try to add it to an idle worker
            match addTaskToWorkList taskName startTime durationFunc workers with
                | Some revisedWorkers ->    // Found a worker
                    step
                        revisedWorkers
                        durationFunc
                        (startTask taskName dependencies) // Make sure we mark it as started
                        startTime
                 | None ->
                    // We get here if there is work to do but we have no idle workers
                    // We have to advance the clock to finish the next task
                        match getNextCompletedTask workers with
                            | None ->
                                startTime
                            | Some completedTask ->
                                step
                                    (idleWorker completedTask.name workers)
                                    durationFunc
                                    (endTask completedTask.name dependencies)
                                    completedTask.endTime





let solvePartTwo (workerCount: int) (durationFunc: char -> int) (dependencies: (char * char list) list) : int =
    printfn "\nPart II has %d workders and %d steps" workerCount dependencies.Length
    let workers = List.replicate workerCount Idle
    dump "workers:" workers

    step workers durationFunc dependencies 0

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

    // solvePartOne dependencies |> ignore

    let durationFunc c = (int c - 65 + 1) + 0
    //let durationFunc c = (int c - 65 + 1) + 60

    printfn "Duration of %c is %d" 'A' (durationFunc 'A')
    solvePartTwo 2 durationFunc dependencies
        |> printfn "Solution to part 2 is %d"

    ()
