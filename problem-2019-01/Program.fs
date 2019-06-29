// Learn more about F# at http://fsharp.org

open System
open System.IO
open FSharp.Data

[<EntryPoint>]
let main argv =
    let url = "https://adventofcode.com/2018/day/1/input"
    let textFile = "problem-2019-01/input.txt"
    let total = File.ReadAllLines textFile
                |> Array.map int
                |> Array.toList
                |> List.sum

    printf "The total is %d\n" total
    0 // return an integer exit code
