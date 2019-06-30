// Learn more about F# at http://fsharp.org

open System.IO


let getData (file: string): array<string> =
    printf "Reading from file %s\n" file
    File.ReadAllLines file

let hasDupsOfLength dupLen (str: string) =
    let result =
        str
            |> Seq.groupBy id
            |> Seq.map (fun (letter, occurrences) -> Seq.length occurrences)
            |> Seq.exists (fun v -> v = dupLen)

    result

let diffCount ((s1,s2)) =
    Seq.zip s1 s2
        |> Seq.map (fun (c1, c2) -> if c1 = c2 then 0 else 1)
        |> Seq.sum

[<EntryPoint>]
let main argv =
    let textFile = "/Users/howard.pinsley/dev/adventofcode/problem-02/input.txt"
    let values = getData textFile |> Array.toList

    printfn "The file contains %d lines" (List.length values)
    let twos = values
                    |> List.filter (hasDupsOfLength 2)
                    |> List.length

    let threes = values
                    |> List.filter (hasDupsOfLength 3)
                    |> List.length

    printfn "The checksum is %d x %d = %d" twos threes (twos * threes)

    let tuples = [  for v1 in values do
                    for v2 in values do
                    if (v1 <> v2) then yield (v1, v2)]

    printfn "I generated %d tuples" (List.length tuples)

    let best = tuples
            |> List.map (fun t -> (diffCount t, t))
            |> List.sortBy (fun t -> fst t)
            |> List.head

    printfn "%A\n" best
    0 // return an integer exit code
