// Learn more about F# at http://fsharp.org

open System.IO
open System.Text.RegularExpressions

type Spec =
    { claim: int
      left: int
      top: int
      width: int
      height: int
    }

let parseClaimFromString str =
    let pattern = "#(\d+) @ (\d+),(\d+): (\d+)x(\d+)";
    let m = Regex.Match(str, pattern)
    if m.Success && m.Groups.Count = 6 then
        Some {  claim = int m.Groups.[1].Value;
                left = int m.Groups.[2].Value;
                top = int m.Groups.[3].Value;
                width = int m.Groups.[4].Value;
                height = int m.Groups.[5].Value
             }
    else
        None

let getData (file: string): array<string> =
    printf "Reading from file %s\n" file
    File.ReadAllLines file

[<EntryPoint>]
let main argv =
    let textFile = "/Users/howard.pinsley/dev/adventofcode/problem-03/input.txt"
    let values = getData textFile
                  |> Array.toList

    let specs = values
                  |> List.choose parseClaimFromString

    printfn "Read %d lines" (List.length values)
    printfn "Read %d specs" (List.length specs)

    let last = List.last specs
    printfn "Sample: %A" last
    0 // return an integer exit code
