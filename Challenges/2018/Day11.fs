module Year2018Day11

open System
open System.IO
open Common
open System.Text.RegularExpressions

let extractHundreds n =
    if (n < 100) then 0
    else
        (n / 100) % 10

let power x y serialNumber =
    let rackId = x + 10
    (rackId * y + serialNumber) * rackId
        |> extractHundreds
        |> (+) -5
let solvePartOne =
    power 122 79 57 |> printfn "%A"
    power 217 196 39 |> printfn "%A"
    power 101 153 71 |> printfn "%A"
    ()

let solve =
    solvePartOne
    ()
