module Year2018Day13

open System
open System.IO
open Common
open System.Text.RegularExpressions

type Cart = {
    row: int;
    col: int;
    dr: int;
    dc: int;
    turnCount: int;
}

type TrackPart =
    | OffTheTrack
    | Intersection
    | HorizontalSegment
    | VerticalSegment
    | Turn

let solvePartOne () =
    printfn "Starting part 1"
    ()

let solvePartTwo () =
    printfn "Starting part 2"
    printfn "End of part 2"
    ()

let prepareInputData (testdata:string[]) =
    let width =
        testdata
            |> Array.map (fun line -> line.Length)
            |> Seq.max

    printfn "The maximum line is %d characters" width

    let padded =
        testdata
            |> Array.map (fun line -> line.PadRight(width))

    let height = testdata.Length

    printfn "We need to create a %d x %d grid." width height

    // let charGrid = Array2D.init height width (fun row col -> sprintf "row%dCol%d" row col)
    let charGrid = Array2D.init height width (fun row col -> ' ')
    let track = Array2D.init height width (fun _ _ -> OffTheTrack)
    padded
        |> Array.iteri (fun row line ->
                            List.ofSeq line
                                |> List.iteri (fun col c -> charGrid.[row,col] <- c)
                                |> ignore
                       )

    // printfn "%A" charGrid

    charGrid
        |> Array2D.iteri (fun row col c ->
                            let trackType =
                                match c with
                                   | '+' -> Intersection
                                   | ' ' -> OffTheTrack
                                   | '-' -> HorizontalSegment
                                   | '|' -> VerticalSegment
                                   | '/' -> Turn
                                   | '\\' -> Turn
                                   | 'v' -> VerticalSegment
                                   | '^' -> VerticalSegment
                                   | '<' -> HorizontalSegment
                                   | '>' -> HorizontalSegment
                                   | c -> failwith "bad char"
                            track.[row,col] <- trackType
                         )

    let cartList =
        seq { for r in 0..height - 1 do
              for c in 0..width - 1 do
              yield (r,c)
            }
            |> Seq.fold (fun carts (r,c) ->
                                match charGrid.[r,c] with
                                    | 'v' -> { row = r; col = c; dr = 1; dc = 0; turnCount = 0} :: carts
                                    | '^' -> { row = r; col = c; dr = -1; dc = 0; turnCount = 0} :: carts
                                    | '<' -> { row = r; col = c; dr = 0; dc = -1; turnCount = 0} :: carts
                                    | '>' -> { row = r; col = c; dr = 0; dc = 1; turnCount = 0} :: carts
                                    | _ -> carts
                         ) []

    (track, cartList)

let solve() =
    let testdata = Common.getChallengeDataAsArray 2018 13
    //let testdata = Common.getSampleDataAsArray 2018 13
    //dump "data" testdata

    let (track, carts) = prepareInputData testdata
    printfn "%A" carts
    solvePartOne()
    //solvePartTwo()
    ()
