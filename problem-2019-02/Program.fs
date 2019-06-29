// Learn more about F# at http://fsharp.org

open System.IO

let rec cycle xs = seq { yield! xs; yield! cycle xs }

let getRepeatingValues (file: string): seq<int> =
    printf "Reading from file %s\n" file

    let numbers = File.ReadAllLines file
                |> Array.map int
                |> Array.toList

    printf "Read %d numbers" (List.length numbers)
    cycle numbers

let rec computeDup (iteration: int) (priorSum: int) (seen: Set<int>) (sequence: seq<int>): int =

    let num = Seq.head sequence
    let nextSum = priorSum + num

    let _ = if iteration % 1000 = 0
            then
                printfn "Iteration: %d is (%d, %d)" iteration num nextSum
            else
                ()


    if (Set.contains nextSum seen)
        then nextSum
        else
            computeDup (iteration + 1) nextSum (Set.add nextSum seen) (Seq.tail sequence)


[<EntryPoint>]
let main argv =
    let textFile = "/Users/howard.pinsley/dev/adventofcode/problem-2019-02/input.txt"
    let sequence = getRepeatingValues textFile
    let firstRepeat = computeDup 0 0 (Set.empty) sequence

    printf "The first repeat is %d" firstRepeat
    0 // return an integer exit code
