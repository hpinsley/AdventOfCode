module Year2022Day01

open System
open System.IO
open Common
open System.Text.RegularExpressions


let solve =

    // let values = Common.getSampleDataAsArray 2022 01
    let values = Common.getChallengeDataAsArray 2022 01
    let calories = values |> Array.map 
                        (fun v -> match v with
                                                    | "" -> -1
                                                    | _ -> parseInt v
                                                )
                        |> List.ofArray

    let terminated = List.append calories [-1]

    let result = 
            terminated 
                |> Seq.fold (fun (elfCals, cumcals) cals  -> 
                                    if (cals = -1) then (List.append elfCals [cumcals], 0)
                                    else (elfCals, cumcals + cals)
                                ) ([], 0)

    let m = List.max (fst result)
    printfn "%A" m

    let top3 = result |> fst |> List.sortDescending |> Seq.take 3 |> Seq.sum
    printfn "%A" top3