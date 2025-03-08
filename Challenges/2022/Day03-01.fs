module Year2022Day03_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions

let letterValue (c:char): int =
    match c with
        | t when t >= 'a' && t <='z' -> (int t) - (int 'a') + 1
        | t when t >= 'A' && t <='Z' -> (int t) - (int 'A') + 27

let solve =

    // let lines = Common.getSampleDataAsArray 2022 03
    let lines = Common.getChallengeDataAsArray 2022 03
    printfn "%A" lines

    let compartmentContents = lines
                                |> Seq.map (fun line -> (line[..line.Length / 2 - 1], line[line.Length / 2..]))
                                |> Seq.map (fun (compartment1, compartment2) -> (Seq.toList compartment1, Seq.toList compartment2))
    printfn "%A" compartmentContents

    let compartmentSets = compartmentContents
                            |> Seq.map (fun (contents1, contents2) -> (Set.ofSeq contents1, Set.ofSeq contents2))

    let totalValue = compartmentSets 
                            |> Seq.map (fun (set1, set2) -> Set.intersect set1 set2)
                            |> Seq.collect (fun s -> Set.toSeq(s))
                            |> Seq.sumBy (fun c -> letterValue c)

    printfn "Part 1: %A" totalValue

    let groups = lines
                    |> Array.mapi (fun i rucksack -> (i / 3, rucksack)) // Assign each set of three to a "group"
                    |> Seq.groupBy (fun (grp, _) -> grp) // Group them
                    |> Seq.map (fun (_, tplSeq) -> Seq.map snd tplSeq) // We don't need the group numbers anymore
                    |> Seq.map (fun sacks -> sacks |> Seq.map (fun sack -> sack |> (Seq.toList >> Set.ofSeq)))

    printfn "%A" groups

    let badges = groups
                |> Seq.map (fun sets -> sets |> Seq.reduce (fun s1 s2 -> Set.intersect s1 s2))
    printfn "%A" badges

    let badgeValues = badges
                        |> Seq.map (fun letters -> letters |> Seq.sumBy (fun c -> letterValue c))

    printfn "%A" badgeValues

    let badgeSum = Seq.sum badgeValues
    printfn "Part 2: %A" badgeSum



    