// Learn more about F# at http://fsharp.org

open System
open FSharp.Data

[<EntryPoint>]
let main argv =
    let url = "https://adventofcode.com/2018/day/1/input"
    printfn "Reading from %s\n" url

    let result = Http.RequestString url

    0 // return an integer exit code

