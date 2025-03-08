module Year2022Day20_Part2

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let DECRYPTION_KEY = 811589153L
// let DECRYPTION_KEY = 1L
let MIX_COUNT = 10

let getInputArray (lines:string[]) : int[] =
    lines
        |> Array.map parseInt

type Cell (value:int64) =
    let mutable pred = None
    let mutable succ = None
    let v = value

    member this.V with get() = v

    member this.Pred
        with get() = 
            pred |> Option.defaultValue (new Cell(-1))
        and
         set(newPred:Cell) =
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
                let newCell = Cell((int64 head) * DECRYPTION_KEY)
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
        // No need to shift more than the ring size
        // Note that since we unlinked our cell, the ring size
        // is n - 1        

        let shiftAmount = cell.V % (int64 ringSize - 1L)

        if (shiftAmount >= 0)
        then
            for _ in seq { 1L .. shiftAmount } do
                successor <- successor.Succ
        else
            for _ in seq { 1L .. -1L * shiftAmount } do
                successor <- successor.Pred
        
        // The cell should go right before its new successor
        cell.Pred <- successor.Pred
        cell.Succ <- successor
        successor.Pred.Succ <- cell
        successor.Pred <- cell

let rec findCellByValue (cell:Cell) (v:int64) : Cell =
    if (cell.V = v)
    then
        cell
    else
        findCellByValue cell.Succ v

let rec findNthSuccessor (cell:Cell) (n:int64) : Cell =
    if (n <= 0)
    then
        cell
    else
        findNthSuccessor cell.Succ (n - 1L)

let solve =
    // let lines = Common.getSampleDataAsArray 2022 20
    let lines = Common.getChallengeDataAsArray 2022 20
    // printAllLines lines
    printfn "There are %d coordinates" lines.Length
    let values = getInputArray lines
    let (ring,pointers) = parseValuesIntoRing values
    //printRing ring values.Length

    printfn "Shifting..."

    for i in seq { 1 .. MIX_COUNT } do
        printfn "Shift operation %d" i
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
