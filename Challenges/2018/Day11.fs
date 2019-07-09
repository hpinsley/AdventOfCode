module Year2018Day11

open System
open System.IO
open Common
open System.Text.RegularExpressions

let extractHundreds n =
    if (n < 100) then 0
    else
        (n / 100) % 10

let power serialNumber x y  =
    let rackId = x + 10
    (rackId * y + serialNumber) * rackId
        |> extractHundreds
        |> (+) -5

let gridPower serialNumber x y  =
    seq { for x1 in x..x+2 do for y1 in y..y+2 -> (x1, y1) }
        |> Seq.sumBy (fun (x,y) -> power serialNumber x y)

let computeCell serialNumber = gridPower serialNumber

let solvePartOne =
    printfn "%A" (gridPower 18 33 45)
    printfn "%A" (gridPower 42 21 61)

    let fn = computeCell 42
    printfn "%A" (fn 21 61)
    ()

let solve =
    solvePartOne
    ()
