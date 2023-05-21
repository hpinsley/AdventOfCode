module Year2022Day13_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type PacketItem =
    | Value of int
    | Nested of PacketItem list

let LBracket = '['
let RBracket = ']'
let Comma = ','

let rec parseNumberPacketItem (buffer:char[]) (startAt:int) : (Option<PacketItem> * int) =
    let maxIndex = buffer.Length - 1
    let mutable intBuffer:char list = []
    let mutable item:PacketItem option = None

    let mutable p = startAt - 1

    while (Option.isNone item && p < maxIndex) do
        p <- p + 1
        let c = buffer[p]
        if (Char.IsDigit(c)) then  
            intBuffer <- List.append intBuffer [c]
        else
            let s = new string [| for c in intBuffer -> c|]
            let v = int s
            item <- Value v |> Some

    (item, p)

let rec parseListPacketItem (buffer:char[]) (startAt:int) (itemParseFunc:char[] -> int -> (Option<PacketItem> * int)): (Option<PacketItem> * int) =
    let mutable packetItems:PacketItem list = []

    let c = buffer[startAt]
    if (c <> LBracket)
    then
        failwith "Expected LBracket"

    let mutable p = startAt + 1
    let mutable isDone = false

    while (not isDone) do
        let (item, newPos) = itemParseFunc buffer p
        match item with
            | Some packetItem ->
                packetItems <- List.append packetItems [packetItem]
                p <- newPos
                if (buffer[p] = Comma)
                then
                    p <- p + 1
                elif (buffer[p] = RBracket)
                then
                    isDone <- true
                    p <- p + 1
                else
                    failwith "Expected closing bracket"
            | None -> 
                p <- newPos
                let bracket = buffer[p]
                if (bracket <> RBracket)
                then
                    failwith "Expected closing right bracket after no item returned"
                
                p <- p + 1
                isDone <- true

    (Some (Nested packetItems), p)

let rec parsePacketItem (buffer:char[]) (startAt:int) : (Option<PacketItem> * int) =
    let c = buffer[startAt]
    if (Char.IsDigit(c))
    then
        parseNumberPacketItem buffer startAt
    else if (c = RBracket)
    then
        (None, startAt)
    else
        parseListPacketItem buffer startAt parsePacketItem


let parsePacket (line:string) : PacketItem =
    let chars = Array.ofSeq line
    let (piOpt, finish) = parsePacketItem chars 0
    match piOpt with
        | Some pi -> 
            //printfn "Finished at %d" finish
            pi
        | None -> failwith "No packet"

let testAllLines (lines:string[]) : unit =
    for line in lines do
    if (line.Length > 1)
    then
        printfn "Parsing %s" line
        let packet = parsePacket line
        printfn "%s = %A" line packet

// Pair up the valid lines, parse them and index the pairs starting at 1
let correlatePairs (lines:string[]) : seq<int * (PacketItem * PacketItem)> =
    let numberedLines =
        lines
            |> Array.filter (fun line -> line.Length > 1)
            |> Array.mapi (fun i line -> (i % 2, line))
    let left = numberedLines |> Seq.filter (fun (i, line) -> i = 0) |> Seq.map (snd >> parsePacket)
    let right = numberedLines |> Seq.filter (fun (i, line) -> i = 1) |> Seq.map (snd >> parsePacket)
    Seq.zip left right |> Seq.mapi (fun i pair -> (i+1, pair))

let rec comparePacketItems (left:PacketItem) (right:PacketItem) : int =
    match left with
        
        | Value lValue ->
            match right with
                | Value rValue -> 
                    if (lValue < rValue)
                    then
                        -1
                    elif (lValue > rValue)
                    then
                        1
                    else
                        0
                | Nested _ -> comparePacketItems (Nested [Value lValue]) right

        | Nested leftList ->
            match right with
                | Value rValue ->
                    comparePacketItems left (Nested [Value rValue])
                | Nested rightList ->
                    printfn "Comparing left list of %d items and right list of %d items" leftList.Length rightList.Length
                    match leftList with
                        | [] ->
                            match rightList with
                                | [] -> 0
                                | head :: tail -> -1
                        | leftItem :: leftRemaining ->
                            match rightList with
                                | [] -> 1
                                | rightItem :: rightRemaining ->
                                    let result = comparePacketItems leftItem rightItem
                                    match result with
                                        | -1 | 1 -> result
                                        | 0 -> comparePacketItems (Nested leftRemaining) (Nested rightRemaining)

                        

let comparePair ((indexNo:int), (left:PacketItem, right:PacketItem)) =
    let result = comparePacketItems left right
    printfn "Comparing index %d: Result: %d" indexNo result
    if (result = -1)
    then
        indexNo
    else
        0

let solve =
    ////let lines = Common.getSampleDataAsArray 2022 13
    let lines = Common.getChallengeDataAsArray 2022 13
    // testAllLines lines

    let pairs = correlatePairs lines |> Array.ofSeq
    for (index, (left, right)) in pairs do
        printfn "%d: Left:%A Right: %A" index left right
    
    let result = pairs |> Seq.map comparePair |> Seq.sum
    printfn "Final result is %d" result

    ()
