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

let rec parsePacketItem (buffer:char[]) (startAt:int) : (Option<PacketItem> * int) =
    let maxIndex = buffer.Length - 1
    let mutable intBuffer:char list = []
    let mutable item:PacketItem option = None

    let mutable p = startAt - 1

    while (Option.isNone item && p < maxIndex) do
        p <- p + 1
        let c = buffer[p]
        if (c >= '0' && c <= '9') then  
            intBuffer <- List.append intBuffer [c]
        else
            let s = new string [| for c in intBuffer -> c|]
            let v = int s
            item <- Value v |> Some

    (item, p)


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
    //let lines = Common.getSampleDataAsArray 2022 13
    //// let lines = Common.getChallengeDataAsArray 2022 13
    //for line in lines do
    //    let packet = parsePacket line
    //    printfn "%s = %A" line packet
    
    let result = parsePacket("[17,18]")
    printfn "%A" result

