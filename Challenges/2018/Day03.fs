module Year2018Day03

open System.IO
open System.Text.RegularExpressions

type Spec =
    { claim: int
      left: int
      top: int
      width: int
      height: int
    }

let specToUnits spec =
  [     for h in 0 .. (spec.height - 1) do
        for w in 0 .. (spec.width - 1) do
        yield (spec.left + w, spec.top + h)
  ]

let parseClaimFromString str =
    let pattern = "#(\d+) @ (\d+),(\d+): (\d+)x(\d+)";
    let m = Regex.Match(str, pattern)
    if m.Success && m.Groups.Count = 6 then
        Some {  claim = int m.Groups.[1].Value;
                left = int m.Groups.[2].Value;
                top = int m.Groups.[3].Value;
                width = int m.Groups.[4].Value;
                height = int m.Groups.[5].Value
             }
    else
        None

let getData (file: string): array<string> =
    printf "Reading from file %s\n" file
    File.ReadAllLines file

let solve =
    let textFile = "InputFiles/input-2018-03.txt"
    let values = getData textFile
                  |> Array.toList

    let specs = values
                  |> List.choose parseClaimFromString

    printfn "Read %d lines" (List.length values)
    printfn "Read %d specs" (List.length specs)

    // If we map each spec to the units consumed, then we can mark using a set each i,j tuple
    // that's been seen.

    let consumed =
      specs
        |> List.collect specToUnits

    let reduced =
      consumed
        |> List.fold (fun state square ->
                        let (seenOnce, seenMultiple) = state
                        let seenMultiple_ = if (Set.contains square seenOnce) then Set.add square seenMultiple else seenMultiple
                        let seenOnce_ = Set.add square seenOnce
                        (seenOnce_, seenMultiple_)
                      )
                      (Set.empty, Set.empty)

    printfn "Consumed has %d elements" (List.length consumed)

    let (seenOnce, seenMultiple) = reduced

    printfn "Seen once: %d" <| Set.count seenOnce
    printfn "Seen multiple: %d (part I answer)" <| Set.count seenMultiple

    let seenOnlyOnce = Set.difference seenOnce seenMultiple
    printfn "Seen only once: %d" <| Set.count seenOnlyOnce

    let solution =
        specs
          |> List.find (fun spec ->
                          spec
                            |> specToUnits
                            |> List.forall (fun used -> Set.contains used seenOnlyOnce)
                        )

    printfn "Solution for part two: %A" solution
