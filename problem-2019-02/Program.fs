// Learn more about F# at http://fsharp.org

open System.IO

let rec cycle xs = seq { yield! xs; yield! cycle xs }

let getRepeatingValues (file: string): seq<int> =
    let numbers = File.ReadAllLines file
                |> Array.map int
                |> Array.toList
    cycle numbers

let rec computeDup (priorSum: int) (seen: Set<int>) (sequence: seq<int>): int =
    let nextSum = match Seq.toList (Seq.take 1 sequence) with
                            | [] -> priorSum
                            | num :: rest -> num + priorSum

    if (Set.contains nextSum seen)
        then nextSum
        else
            computeDup nextSum (Set.add nextSum seen) sequence


[<EntryPoint>]
let main argv =
    let textFile = "problem-2019-02/input.txt"
    let sequence = getRepeatingValues textFile
    let firstRepeat = computeDup 0 (Set.empty) sequence

    printf "The first repeat is %d" firstRepeat
    0 // return an integer exit code
