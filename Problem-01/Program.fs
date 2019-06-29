// Learn more about F# at http://fsharp.org

open System.IO


let getRepeatingValues (file: string): array<int> =
    printf "Reading from file %s\n" file

    File.ReadAllLines file
        |> Array.map int

let rec computeDup (iteration: int) (priorSum: int) (seen: Set<int>) (values: array<int>): int =

    let length = Array.length values
    let num = Array.get values (iteration % length)
    let nextSum = priorSum + num

    let _ = if iteration % 1000 = 0
            then
                printfn "Iteration: %d is (%d, %d)" iteration num nextSum
            else
                ()


    if (Set.contains nextSum seen)
        then nextSum
        else
            computeDup (iteration + 1) nextSum (Set.add nextSum seen) values


[<EntryPoint>]
let main argv =
    let textFile = "/Users/howard.pinsley/dev/adventofcode/problem-01/input.txt"
    let values = getRepeatingValues textFile
    let firstRepeat = computeDup 0 0 (Set.empty) values

    printf "The first repeat is %d" firstRepeat
    0 // return an integer exit code
