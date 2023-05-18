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

type Packet = PacketItem list

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


let parsePacket (line:string) : Packet =
    let chars = Array.ofSeq line
    if (chars[0] <> LBracket || chars[chars.Length - 1] <> RBracket)
    then
        failwith "Packets must be surrounded by ()"

    let mutable packet:PacketItem list = []

    let mutable p = 1
    let mutable isDone = false

    while (not isDone) do
        let (item, newPos) = parsePacketItem chars p
        match item with
            | Some packetItem ->
                packet <- List.append packet [packetItem]
                p <- newPos
                if (chars[p] = Comma)
                then
                    p <- p + 1
                elif (chars[p] = RBracket)
                then
                    isDone <- true

            | None -> isDone <- true

    packet

let solve =
    ////let lines = Common.getSampleDataAsArray 2022 13
    let lines = Common.getChallengeDataAsArray 2022 13
    for line in lines do
        if (line.Length > 1)
        then
            printfn "Parsing %s" line
            let packet = parsePacket line
            printfn "%s = %A" line packet
    
    //let result = parsePacket("[17,18]")
    //let result = parsePacket("[[18,19]]")
    //let result = parsePacket("[[]]")
    //let result = parsePacket("[[],[3,4,[5,6]]]")
    //printfn "%A" result

