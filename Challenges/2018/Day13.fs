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

type IntersectionChoice = Left | Straight | Right

type TrackPart =
    | OffTheTrack
    | Intersection
    | HorizontalSegment
    | VerticalSegment
    | Turn

let getNextTurn turnCount =
    match turnCount % 3 with
        | 0 -> Left
        | 1 -> Straight
        | 2 -> Right

let turnRight (cart:Cart) =
    { cart with
        row = cart.row + cart.dc;
        col = cart.col - cart.dr;
        dr = cart.dc;
        dc = -cart.dr;
    }

let turnLeft (cart:Cart) =
    { cart with
        row = cart.row - cart.dc;
        col = cart.col + cart.dr;
        dr = -cart.dc;
        dc = cart.dr;
    }

let goStraight (cart:Cart) =
    {
        cart with
            row = cart.row + cart.dr;
            col = cart.col + cart.dc;
    }

let moveThroughIntersection (cart:Cart) =
    match getNextTurn cart.turnCount with
        | Left ->
            { turnLeft cart with turnCount = cart.turnCount + 1; }
        | Straight ->
            { goStraight cart with turnCount = cart.turnCount + 1; }
        | Right ->
            { turnRight cart with turnCount = cart.turnCount + 1; }

let moveThroughHorizontalSegment (cart:Cart) =
    { cart with col = cart.col + cart.dc }

let moveThroughVerticalSegment (cart:Cart) =
    { cart with row = cart.row + cart.dr }

let turn (cart:Cart) =
    if (cart.dc < 0) then

let move (track:TrackPart[,]) (movedCarts:Cart list) (cart:Cart) =
    let r = cart.row
    let c = cart.col
    let trackPart = track.[r,c]

    let movedCart =
        match trackPart with
            | OffTheTrack -> failwith "Off the track!"
            | Intersection -> moveThroughIntersection cart
            | HorizontalSegment -> moveThroughHorizontalSegment cart
            | VerticalSegment -> moveThroughVerticalSegment cart
            | Turn -> turn cart

    movedCarts

let solvePartOne (track:TrackPart[,]) (carts:Cart list) =
    printfn "Starting part 1"
    carts
        |> List.sortBy (fun cart -> (cart.row, cart.col))
        |> List.fold (move track) []
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
    solvePartOne track carts
    //solvePartTwo()
    ()
