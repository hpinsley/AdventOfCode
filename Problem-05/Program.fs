// Learn more about F# at http://fsharp.org
open System
open System.IO
open System.Text.RegularExpressions

let cancels (c1:char) (c2:char) =
    c1 <> c2 && Char.IsLetter(c1) && Char.ToLower c1 = Char.ToLower c2

let rec reactString (prevChar: char option) (chars:char list) =

    seq {
        match (prevChar, chars) with
            | (Some p, []) ->
                yield p
            | (Some p, c :: rest)->
                if (cancels p c) then
                    yield! reactString None rest
                else
                    yield p
                    yield! reactString (Some c) rest
            | (None, c :: rest) ->
                    yield! reactString (Some c) rest
            | (None, []) ->
                ()
    }

let getData (file: string): string =
    printf "Reading from file %s\n" file
    File.ReadAllText file

let rec processString (str:String) =
    let reduced =
            str
                |> Seq.toList
                |> reactString None
                |> Seq.toList
                |> String.Concat
    if (reduced.Length < str.Length)
    then
        printfn "Reduced to %d" reduced.Length
        processString reduced
    else
        str


[<EntryPoint>]
let main argv =
    let textFile = "/Users/howard.pinsley/dev/adventofcode/problem-05/input.txt"
    let testdata = getData textFile
    let sampledata = "dabAcCaCBAcCcaDA"
    let data = testdata

    printfn "Read data of length %d" (String.length data)
    let reduced = processString data
    printfn "Reduced has length %d" reduced.Length

    0 // return an integer exit code
