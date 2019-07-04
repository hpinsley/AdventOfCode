module Year2018Day07

open System
open System.IO
open Common

let solve =
    //let testdata = Common.getChallengeDataAsArray 2018 7
    let testdata = Common.getSampleDataAsArray 2018 7
    dump "data" testdata
    let pairs = testdata
                    |> List.ofArray
                    |> List.map (fun line -> (line.[5], line.[36]))
                    |> dump "pairs"
    ()
