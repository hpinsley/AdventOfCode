// Learn more about F# at http://fsharp.org
open System
open System.IO
open System.Text.RegularExpressions

let cancels (c1:char) (c2:char) =
    c1 <> c2 && Char.IsLetter(c1) && Char.ToLower c1 = Char.ToLower c2

let rec reactString (prevChar: char option) (chars:Char[]) =

    let nextChar = if chars.Length = 0 then None else Some chars.[0]
    let n = chars.Length

    if (n < 100)
    then
        printfn "Substring is %d chars" n

    seq {
        match (prevChar, nextChar) with
            | (Some p, None) ->
                yield p
            | (Some p, Some c)->
                if (cancels p c) then
                    yield! reactString None (chars.[1..n-1])
                else
                    yield p
                    yield! reactString (Some c) (chars.[1..n-1])
            | (None, Some c) ->
                    yield! reactString (Some c) (chars.[1..n-1])
            | (None, None) ->
                ()
    }

let getData (file: string): string =
    printf "Reading from file %s\n" file
    File.ReadAllText file

let rec processString (str:String) =
    let reduced =
            str.ToCharArray()
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
