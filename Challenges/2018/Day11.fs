module Year2018Day11

open System
open System.IO
open System.Collections.Generic
open Common
open System.Text.RegularExpressions

let MaxGrid = 300

let extractHundreds n =
    if (n < 100) then 0
    else
        (n / 100) % 10

let powerHard serialNumber x y  =
    let rackId = x + 10
    (rackId * y + serialNumber) * rackId
        |> extractHundreds
        |> (+) -5

let mutable hardCalls = 0
let mutable hardFullGridCalls = 0
let mutable softFullGridCalls = 0

let memoizePowerFunction () =
    let cache = Dictionary<(int*int*int),int>()
    printfn "Created cache"
    let f sn x y =
        let t = (sn,x,y)
        if (cache.ContainsKey(t)) then
            cache.[t]
        else
            hardCalls <- hardCalls + 1
            let v = powerHard sn x y
            cache.[t] <- v
            v
    f

let powerFun = memoizePowerFunction()

let sizeGenerator maxSize =
    seq { for s in 1 .. maxSize do
            printfn "Starting size %d" s
            yield s
        }

let gridPower serialNumber x y size =
    seq { for x1 in x..x+(size-1) do for y1 in y..y+(size-1) -> (x1, y1) }
        |> Seq.sumBy (fun (x,y) -> powerFun serialNumber x y)

//let computeCell serialNumber = gridPower serialNumber

let solvePartOne () =
    let gridSerialNumber = 9435
    let (mx, my, ms) =
        seq {
            for s in 3..3 do
            for x in 1..(MaxGrid-(s-1)) do
            for y in 1..(MaxGrid-(s-1)) -> (x,y,s)
        } |> Seq.maxBy (fun (x,y,s) -> gridPower gridSerialNumber x y s)

    printfn "Part I: %A (size %d) with value %d" (mx,my) ms (gridPower gridSerialNumber mx my ms)
    ()

let solvePartTwo () =
    let gridSerialNumber = 42

    // Inner function to compute gridpower of size s at x,y.  First see if we have
    // memoized the value of size s-1 at the same corner.  If so, we only need to
    // compute the power for the bottom and right edges of the grid of size s
    let folder (dict:Dictionary<(int * int * int), int>) v =
        let (x,y,s) = v
        let priorKey = (x,y,s-1)

        // let powerFun = memoizePowerFunction()
        let newValueForState =
            if dict.ContainsKey(priorKey)
            then
                softFullGridCalls <- softFullGridCalls + 1
                dict.[priorKey]
                        +
                        (
                            seq { for x1 in x..x+s-1 -> (x1, y + s - 1) }
                                |> Seq.sumBy (fun (x0, y0) -> powerFun gridSerialNumber x0 y0)
                        )
                        +
                        (
                            // Don't double count the bottom right corner
                            seq { for y1 in y..y+s-2 -> (x + s - 1, y1) }
                                |> Seq.sumBy (fun (x0, y0) -> powerFun gridSerialNumber x0 y0)
                        )
            else
                hardFullGridCalls <- hardFullGridCalls + 1
                let computed = gridPower gridSerialNumber x y s
                computed

        dict.Add(v, newValueForState)
        dict

    let initialDict = new Dictionary<(int * int * int), int>()

    let finalValues =
        seq {
            for s in sizeGenerator 20 do
            for x in 1..(MaxGrid-(s-1)) do
            for y in 1..(MaxGrid-(s-1)) -> (x,y,s)
        } |> Seq.fold folder initialDict

    let ((mx, my, ms),mv) = finalValues
                                |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))
                                |> Seq.maxBy (fun ((_,_,_),v) -> v)

    printfn "Part II:%A (size %d) with value %d" (mx,my) ms mv
    ()

let solve () =
    //solvePartOne()
    solvePartTwo()
    printfn "done with %d hard calls and %d hard full grid calls and %d soft" hardCalls hardFullGridCalls softFullGridCalls
    ()
