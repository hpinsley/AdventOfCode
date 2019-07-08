module Year2018Day10

open System
open System.IO
open Common
open System.Text.RegularExpressions

let solvePartOne  =
    ()

let solvePartTwo  =
    ()

let parseLine (line:string) : ((int * int) * (int * int)) =
    let pattern = "position=< *(\d+), *(\d+)> velocity=< *(\d+), *(\d+)>"
    let matchResult = Regex.Match(line, pattern)
    ((0,0),(0,0))

let solve =
    //let testdata = Common.getChallengeDataAsArray 2018 10
    let testdata = Common.getSampleDataAsArray 2018 10
    dump "data" testdata

    let data = testdata
                    |> List.ofArray
                    |> List.map parseLine

    dump "parsed" data

    solvePartOne
    //solvePartTwo
    ()
