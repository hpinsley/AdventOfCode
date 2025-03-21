﻿module Year2022Day20_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let getInputArray (lines:string[]) : int[] =
    lines
        |> Array.map parseInt

type Cell (value:int) =
    let mutable pred = None
    let mutable succ = None
    let v = value

    member this.V with get() = v

    member this.Pred
        with get() = pred |> Option.defaultValue (new Cell(-1))
        and set(newPred:Cell) = 
            pred <- Some newPred

    member this.Succ
        with
                get() = succ |> Option.defaultValue (new Cell(-1))
        and
                set(newSucc:Cell) = succ <- Some newSucc

(*
    Construct 2 things.  A doubly linked list of cells that also
    connects the first and last.

    In addition, an array of pointers to the Cells that we can use
    to keep track of the original order so we can iterate, find
    each cell that needs moving, and do the actual moving in the ring
    to avoid all kinds of array shifting.
*)
let parseValuesIntoRing (values:int[]) : Tuple<Cell,Cell[]> =
    let valueList = List.ofArray values
    let mutable firstCell = None
    let mutable lastCell = None
    let mutable pointers = []

    let rec addToList (parent:Cell option) (v:int list) =
        match v with
            | head :: tail ->
                let newCell = Cell(head)
                pointers <- newCell :: pointers
                match parent with
                    | Some p ->
                        p.Succ <- newCell
                        newCell.Pred <- p
                    | None -> ()
                match firstCell with
                    | None -> firstCell <- Some newCell
                    | Some c -> ()

                lastCell <- Some newCell
                addToList (Some newCell) tail
            | [] -> ()
    
    addToList None valueList

    // Link the first and last to complete the ring and return the first
    match firstCell with
        | Some f ->
                match lastCell with
                    | Some last ->
                        last.Succ <- f
                        f.Pred <- last
                        let p = pointers |> List.rev |> Array.ofList
                        (f, p)
                    | None -> failwith "No last cell"
        | None -> failwith "No first cell set"

let printRing (c:Cell) (ringSize:int) =
    let mutable current = c
    for _ in seq { 1 .. ringSize } do
        printfn "Cell %d" current.V
        current <- current.Succ

let unlink (cell:Cell) : unit =
    let pred = cell.Pred
    let succ = cell.Succ
    pred.Succ <- succ
    succ.Pred <- pred

let performShifts (cellFinder:Cell[]) : unit =
    let ringSize = cellFinder.Length
    for i in seq { 0 .. ringSize - 1} do
        let cell = cellFinder[i]
        let mutable successor = cell.Succ
        unlink cell
        // Find the new spot for this cell

        if (cell.V > 0)
        then
            for _ in seq { 1 .. abs cell.V } do
                successor <- successor.Succ
        else
            for _ in seq { 1 .. abs cell.V } do
                successor <- successor.Pred

        // The cell should go right before its new successor
        cell.Pred <- successor.Pred
        cell.Succ <- successor
        successor.Pred.Succ <- cell
        successor.Pred <- cell

let rec findCellByValue (cell:Cell) (v:int) : Cell =
    if (cell.V = v)
    then
        cell
    else
        findCellByValue cell.Succ v

let rec findNthSuccessor (cell:Cell) (n:int) : Cell =
    if (n <= 0)
    then
        cell
    else
        findNthSuccessor cell.Succ (n - 1)

let solve =
    // let lines = Common.getSampleDataAsArray 2022 20
    let lines = Common.getChallengeDataAsArray 2022 20
    // printAllLines lines
    printfn "There are %d coordinates" lines.Length
    let values = getInputArray lines
    let (ring,pointers) = parseValuesIntoRing values
    //printRing ring values.Length

    printfn "Shifting..."
    performShifts pointers
    //printRing ring values.Length
    printfn "Finding anchor..."
    let anchor = findCellByValue ring 0
    printfn "Computing result..."
    let c1 = findNthSuccessor anchor 1000
    let c2 = findNthSuccessor anchor 2000
    let c3 = findNthSuccessor anchor 3000
    let s = c1.V + c2.V + c3.V
    printfn "The sum is %d" s
    ()
