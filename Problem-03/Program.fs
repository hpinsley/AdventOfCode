// Learn more about F# at http://fsharp.org

open System.IO


let getData (file: string): array<string> =
    printf "Reading from file %s\n" file
    File.ReadAllLines file


[<EntryPoint>]
let main argv =
    let textFile = "/Users/howard.pinsley/dev/adventofcode/problem-03/input.txt"
    let values = getData textFile |> Array.toList

    printfn "The file contains %d lines" (List.length values)
    0 // return an integer exit code
