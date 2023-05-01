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

let solve =

    let lines = Common.getSampleDataAsArray 2022 06
    // let lines = Common.getChallengeDataAsArray 2022 06
    printfn "%A" lines
