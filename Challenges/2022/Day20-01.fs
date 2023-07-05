module Year2022Day20_Part1

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
            with
                get() = pred |> Option.defaultValue (new Cell(-1))
            and
                set(newPred:Cell) = pred <- Some newPred

    member this.Succ
            with
                get() = succ |> Option.defaultValue (new Cell(-1))
            and
                set(newSucc:Cell) = succ <- Some newSucc

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
                
let solve =
    let lines = Common.getSampleDataAsArray 2022 20
    // let lines = Common.getChallengeDataAsArray 2022 20
    // printAllLines lines
    printfn "There are %d coordinates" lines.Length
    let values = getInputArray lines
    let (ring,pointers) = parseValuesIntoRing values
    printRing ring values.Length
    ()
