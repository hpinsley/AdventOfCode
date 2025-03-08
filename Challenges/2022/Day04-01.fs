module Year2022Day04_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions

let isContainedBy (superset:int * int) (subset:int * int): bool =
    (fst subset >= fst superset) && (snd subset <= snd superset)

let overlaps (s1:int * int) (s2:int * int): bool =
    (fst s1 <= snd s2) && (snd s1 >= fst s2)

let parseLine (line:string) : ((int * int) * (int * int)) =
    let pattern = "(\d+)-(\d+),(\d+)-(\d+)"
    let matchResult = Regex.Match(line, pattern)
    let a11 = int matchResult.Groups.[1].Value
    let a12 = int matchResult.Groups.[2].Value
    let a21 = int matchResult.Groups.[3].Value
    let a22 = int matchResult.Groups.[4].Value
    ((a11,a12),(a21,a22))

let solve =

    // let lines = Common.getSampleDataAsArray 2022 04
    let lines = Common.getChallengeDataAsArray 2022 04
    printfn "%A" lines  
    let parsed = lines |> Seq.map parseLine |> List.ofSeq
    printfn "%A" parsed


    let hasFullOverlaps = parsed |> List.filter (fun t -> isContainedBy (fst t) (snd t) || isContainedBy (snd t) (fst t))
    printfn "%A" hasFullOverlaps

    printfn "%A pairs have assignments where one is completly covered by the other" hasFullOverlaps.Length

    let hasPartialOverlaps = parsed |> List.filter (fun t -> overlaps (fst t) (snd t))
    printfn "%A pairs have assignments where one overlaps the other" hasPartialOverlaps.Length



