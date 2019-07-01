module Year2018Day05

open System
open System.IO
open System.Text.RegularExpressions

// let remainingPolymer =
//     let processUnit polymer ch =
//         match polymer with
//         | x :: xs when abs (ch - x) = 32 -> xs
//         | xs -> ch :: xs
//     Seq.fold processUnit []

let cancels (c1:char) (c2:char) =
    // 97-65 = 32
    let diff = ((int c1) - (int c2))
    diff = 32 || diff = -32

// This is pretty slow.  The fold solution above is way better.
let rec reactString (prevChar: char option) (chars:char list) =
    seq {
        match prevChar with
            | Some p ->
                match chars with
                    | c :: rest ->
                        if (cancels p c) then
                            yield! reactString None rest
                        else
                            yield p
                            yield! reactString (Some c) rest
                    | [] ->
                        yield p
            | None ->
                match chars with
                    | [] ->
                        ()
                    | c :: rest ->
                        yield! reactString (Some c) rest
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
        // printfn "Reduced to %d" reduced.Length
        processString reduced
    else
        str

let testUnitType (data:string) (unitType:char) =
    printfn "Testing unit %A" unitType
    let lowerc = Char.ToLower unitType
    let upperc = Char.ToUpper unitType

    let newData = data
                    |> Seq.toList
                    |> Seq.filter (fun c -> c <> lowerc && c <> upperc)
                    |> Array.ofSeq
                    |> String.Concat

    printfn "After eliminating upper and lower %c we have %d chars" unitType newData.Length

    let reduced = processString newData
    (unitType, reduced.Length)

let solve =
    let textFile = "/Users/howard.pinsley/dev/adventofcode/problem-05/input.txt"
    let testdata = getData textFile
    let sampledata = "dabAcCaCBAcCcaDA"
    let data = testdata

    printfn "Read data of length %d" (String.length data)
    let reduced = processString data
    printfn "Reduced has length %d" reduced.Length

    let unitTypes = ['a'..'z']
    let best =
        unitTypes
            |> List.map (testUnitType data)
            |> List.minBy (fun (c, codelen) -> codelen)

    printfn "Best is %A" best

    0 // return an integer exit code
