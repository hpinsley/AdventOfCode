module Year2024Day5_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type UpdateList = int[]

let middleElement (updateList:UpdateList) : int =
    if updateList.Length % 2 = 0
    then
        raise (Exception("Did not expect an update list an even number of items.  There is not 'middle' element."))
    else
        updateList[updateList.Length / 2]
        
let isValidUpdateList (isValidSuccessor:int->int->bool) (updateList:UpdateList) : bool =
    let mutable result = true
    for position in seq {1..updateList.Length - 1} do
        let successor = updateList[position]
        for j in seq {0..position - 1} do
            let predecessor = updateList[j]
            result <- result && (isValidSuccessor predecessor successor)

    result
       
let moveItemToValidLocation (isValidSuccessor:int->int->bool) (updateList:UpdateList) (lastIndex:int): unit =
    let mutable i = lastIndex

    while (i > 0) do
        let priorIndex = i - 1
        let last = updateList[i]
        let prior = updateList[priorIndex]

        if not (isValidSuccessor prior last) then // Swap the elements
            Array.set updateList priorIndex last
            Array.set updateList i prior
        else
            ()
        i <- i - 1

let rec reorderUpdateList (isValidSuccessor:int->int->bool) (updateList:UpdateList) (itemsToReorder:int): unit =
    if (itemsToReorder > 1)
    then
        reorderUpdateList isValidSuccessor updateList (itemsToReorder - 1) |> ignore
        moveItemToValidLocation isValidSuccessor updateList (itemsToReorder - 1)
    else
        ()

let solve =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    // let lines = Common.getSampleDataAsArray 2024 5
    let lines = Common.getChallengeDataAsArray 2024 5

    let blankLineIndex = Array.findIndex (fun line -> line = "") lines
    let topSection = lines[0..blankLineIndex - 1]
    let bottomSection = lines[blankLineIndex+1..]

    // Build a map of page numbers which contain a set of mandatory predecessors
    let mandatoryPrecessors:Map<int,Set<int>> = topSection |> Array.map (fun line -> line.Split [|'|'|])
                                                        |> Array.map (fun items -> (parseInt items[0], parseInt items[1]))
                                                        |> Array.groupBy snd
                                                        |> Array.map (fun tpl -> (fst tpl, Array.map (fun t -> fst t) (snd tpl)))
                                                        |> Array.map (fun tpl -> (fst tpl, Set.ofArray (snd tpl)))
                                                        |> Map.ofArray
        

    // Build a handy function to use the map to validate a successor to a predecessor    
    let isValidSuccessor (pred:int) (succ:int): bool =
        // If the succ is in the list of mandatory predecessors of the pred, then it is invalid
        match Map.tryFind pred mandatoryPrecessors with
                                                    | Some setOfPredecessors -> not (Set.contains succ setOfPredecessors)
                                                    | None -> true


    let updateLists:UpdateList[] = bottomSection 
                                    |> Array.map (fun line -> line.Split [|','|] 
                                                                |> Array.map (fun chunk -> parseInt chunk)
                                                 )


    let updateCheck:UpdateList->bool = isValidUpdateList isValidSuccessor

    let validUpdateLists = updateLists |> Array.filter updateCheck
    let updateSum = validUpdateLists |> Array.sumBy middleElement

    let part1Time = stopWatch.ElapsedMilliseconds

    printfn "Part 1: There are %d valid update lists.  Check sum is %d" validUpdateLists.Length updateSum

    stopWatch.Restart() |> ignore

    let invalidUpdateLists = updateLists |> Array.filter (fun ul -> not (isValidUpdateList isValidSuccessor ul))

    for v in invalidUpdateLists do
        reorderUpdateList isValidSuccessor v v.Length

    let invalidUpdateSum = invalidUpdateLists |> Array.sumBy middleElement
    let part2Time = stopWatch.ElapsedMilliseconds;

    printfn "Part 2: There are %d invalid update lists.  Corrected sum is %d" invalidUpdateLists.Length invalidUpdateSum

    printfn "Timings.  Part 1: %dµs, Part 2: %dµs" part1Time part2Time
    ()