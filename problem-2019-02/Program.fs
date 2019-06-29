// Learn more about F# at http://fsharp.org

open System
open System.IO
open FSharp.Data



let getSum (file: string): int =
    let total = File.ReadAllLines file
                |> Array.map int
                |> Array.toList
                |> List.sum
    total

[<EntryPoint>]
let main argv =
    let textFile = "problem-2019-02/input.txt"
    let total = getSum textFile
    printf "The total for #3 is %d\n" total
    0 // return an integer exit code
