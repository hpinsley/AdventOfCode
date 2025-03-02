module Year2024Day5_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type OrderingRule =
    {
        predecessor: int;
        successor: int;
    }

type UpdateList = int[]

let solve =
    let lines = Common.getSampleDataAsArray 2024 5
    // let lines = Common.getChallengeDataAsArray 2024 5

    let blankLineIndex = Array.findIndex (fun line -> line = "") lines
    let topSection = lines[0..blankLineIndex - 1]
    let bottomSection = lines[blankLineIndex+1..]

    let orderingRules = topSection |> Array.map (fun line -> 
                                                    let parts = line.Split [|'|'|]
                                                    {
                                                        predecessor = parseInt parts[0];
                                                        successor = parseInt parts[1];
                                                    })
    // printfn "%A" orderingRules

    let updateLists:UpdateList[] = bottomSection 
                                    |> Array.map (fun line -> line.Split [|','|] 
                                                                |> Array.map (fun chunk -> parseInt chunk)
                                                 )
    ()