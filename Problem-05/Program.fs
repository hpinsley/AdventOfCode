// Learn more about F# at http://fsharp.org
open System
open System.IO
open System.Text.RegularExpressions

let cancels (c1:char) (c2:char) =
    c1 <> c2 && Char.IsLetter(c1) && Char.ToLower c1 = Char.ToLower c2

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

[<EntryPoint>]
let main argv =
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
