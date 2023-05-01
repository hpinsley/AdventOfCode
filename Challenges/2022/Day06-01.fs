module Year2022Day06_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions

type Instruction = {
    CratesToMove: int;
    FromCrate: int;
    ToCrate: int
}

let isMarker (s:string) : bool =
    let sorted = s |> Seq.sort |> Array.ofSeq |> System.String
    let distinct = sorted |> Seq.distinct |> Seq.sort |> Array.ofSeq |> System.String
    let result = distinct.Equals(sorted)
    //printfn "%A %A %A %A" s sorted distinct result
    result

let solve =

    let markerChars = 14

    // let lines = Common.getSampleData 2022 06
    let lines = Common.getChallengeData 2022 06
    printfn "%A" lines

    let testLines = [markerChars..lines.Length] |> List.map (fun len -> lines[0..len-1])
    let tuples = testLines |> List.map (fun s -> (s, s[s.Length - markerChars..]))

    let firstMatch = tuples |> List.tryFind (fun t -> isMarker (snd t))
    printfn "%A" firstMatch
    match firstMatch with
        | None -> failwith("no match found")
        | Some (line, marker) -> printfn "Found line %s with marker %s; length of line: %d" line marker line.Length 
