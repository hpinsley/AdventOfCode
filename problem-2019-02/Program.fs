// Learn more about F# at http://fsharp.org

open System
open System.IO
open FSharp.Data

let getValues (file: string): (unit -> int list) =
    let numbers = File.ReadAllLines file
                |> Array.map int
                |> Array.toList

    let f = fun () -> numbers
    f


let getSum (file: string): int =
    let valFunc = getValues file
    let total = valFunc()
                |> List.sum
    total

[<EntryPoint>]
let main argv =
    let textFile = "problem-2019-02/input.txt"
    let total = getSum textFile
    printf "The total for #4 is %d\n" total
    0 // return an integer exit code
