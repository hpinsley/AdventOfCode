module Year2025Day9_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open System.Diagnostics

type LOC_TYPE = UInt64

type TileLocation = {
    x: LOC_TYPE
    y: LOC_TYPE
}

let solve =
    let stopWatch = Stopwatch.StartNew()

    let lines = Common.getSampleDataAsArray 2025 9
    // let lines: string array = Common.getChallengeDataAsArray 2025 9

    printfn "%A" lines
    let locations = lines
                        |> Array.map (fun (s:string) ->
                                        let split = s.Split(",")
                                        { 
                                            x = UInt64.Parse(split[0])
                                            y = UInt64.Parse(split[1])
                                        }
                                     )

    printfn "%A" locations
    ()