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

let parsePacket (line:string) : Packet =
    [Value 3]

let solve =
    let lines = Common.getSampleDataAsArray 2022 13
    // let lines = Common.getChallengeDataAsArray 2022 13
    for line in lines do
        let packet = parsePacket line
        printfn "%s = %A" line packet


