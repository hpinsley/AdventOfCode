// Learn more about F# at http://fsharp.org

open System.IO
open System.Text.RegularExpressions

let getData (file: string): array<string> =
    printf "Reading from file %s\n" file
    File.ReadAllLines file


[<EntryPoint>]
let main argv =
    let textFile = "/Users/howard.pinsley/dev/adventofcode/problem-05/input.txt"
    let values = getData textFile
                  |> Array.toList

    printfn "(5) Read %d lines" (List.length values)

    0 // return an integer exit code
