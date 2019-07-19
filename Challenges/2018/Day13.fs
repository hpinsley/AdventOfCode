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
    | Turn of Char

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

let turn (cart:Cart) (c:Char) =
    let (dr, dc) =
        match c with
            | '/' -> (-cart.dc, -cart.dr)
            | '\\' -> (cart.dc, cart.dr)

    { cart with
        row = cart.row + dr;
        col = cart.col + dc;
        dr = dr;
        dc = dc;
    }

type GameState = {
    track: TrackPart[,];
    unmovedCarts: Cart list;
    movedCarts: Cart list;
    tickCount: int;
    collisions: Cart list;
}

let orderCarts (carts:Cart list) : (Cart list) =
    carts |> List.sortBy (fun cart -> (cart.row, cart.col))

let moveCart (cart: Cart) (trackPart:TrackPart) : Cart =
    match trackPart with
        | OffTheTrack -> failwith "Off the track!"
        | Intersection -> moveThroughIntersection cart
        | HorizontalSegment -> moveThroughHorizontalSegment cart
        | VerticalSegment -> moveThroughVerticalSegment cart
        | Turn c -> turn cart c

let move (track:TrackPart[,]) (movedCarts:Cart list) (cart:Cart) =
    let r = cart.row
    let c = cart.col
    let trackPart = track.[r,c]

    let movedCart = moveCart cart trackPart
    movedCart :: movedCarts

let tick (track:TrackPart[,]) (carts:Cart list) =
    carts
        |> orderCarts
        |> List.fold (move track) []

let findCollisions (carts:Cart list) =
    let collisions = carts
                        |> List.groupBy (fun c -> (c.row, c.col))
                        |> List.choose (fun ((r, c),carts) ->
                                            if carts.Length > 1
                                                then Some (r,c)
                                                else None
                                        )
    collisions

let rec tickUntilCollison (track:TrackPart[,]) (carts:Cart list) iteration =

    printfn "Iteration %d" iteration
    carts
        |> List.map (fun c -> (c.row,c.col))
        |> List.map (fun t -> (sprintf "%d,%d,%A" (fst t) (snd t) track.[fst t,snd t]))
        |> String.concat ";"
        |> printfn "I:%d %s" iteration
        |> ignore

    let collisions = findCollisions carts

    match collisions with
        | [] ->
                let movedCarts = tick track carts
                tickUntilCollison track movedCarts (iteration + 1)

        | collided -> collided

let haveCollided c1 c2 =
    c1.row = c2.row && c1.col = c2.col

let findCollisionsInLists (movedCart:Cart) (unmoved:Cart list) (moved:Cart list) =
    let c1 =
        unmoved
            |> List.filter (haveCollided movedCart)
    let c2 =
        moved
            |> List.filter (haveCollided movedCart)

    List.concat [c1; c2]

let filterCollisionsOutOfLists (movedCart:Cart) (unmoved:Cart list) (moved:Cart list) =
    let cleanCart = not << (haveCollided movedCart)

    let u =
        unmoved
            |> List.filter cleanCart
    let m =
        moved
            |> List.filter cleanCart
    let collisionDetected = u.Length <> unmoved.Length || m.Length <> moved.Length

    if (collisionDetected) then printfn "Collision detected at %A" movedCart

    (collisionDetected, u, m)

let rec solver (state:GameState) =
    match state.unmovedCarts with
        | [] ->
            printfn "Moving to tick count %d" (state.tickCount + 1)
            solver {
                state with
                    unmovedCarts = orderCarts state.movedCarts;
                    movedCarts = [];
                    tickCount = state.tickCount + 1
            }

        | cart :: remainingUnmoved ->
            let trackPart = state.track.[cart.row, cart.col]
            let movedCart = moveCart cart trackPart
            let collisions = findCollisionsInLists movedCart remainingUnmoved state.movedCarts
            match collisions with
                | [] ->
                    solver {
                        state with
                            unmovedCarts = remainingUnmoved;
                            movedCarts = movedCart :: state.movedCarts;
                    }
                | collided ->
                    { state with collisions = collided }

let solvePartOne (track:TrackPart[,]) (carts:Cart list) =
    let state = {
        track = track;
        unmovedCarts = carts;
        movedCarts = [];
        tickCount = 0;
        collisions = []
    }

    let solved = solver state
    printfn "%A" solved.collisions
    ()

let solvePartOneOld (track:TrackPart[,]) (carts:Cart list) =
    printfn "Starting part 1"
    // carts
    //     |> tick track
    //     |> printfn "%A"

    let collisions = tickUntilCollison track carts 0
    match collisions with
        | [collision] ->
                        let (y, x) = collision
                        printfn "Solution is %A" (x,y)
        | other ->
                        printfn "Found %d collisions" (other.Length)
    ()

let getCartChar (cart: Cart) : char =
    match cart.dr with
        | -1 -> '^'
        | 1 -> 'v'
        | _ -> match cart.dc with
                | -1 -> '<'
                | 1 -> '>'
                | _ -> '?'

let getTrackChar (part:TrackPart) : char =
    match part with
        | OffTheTrack -> ' '
        | Intersection -> '+'
        | HorizontalSegment -> '-'
        | VerticalSegment -> '|'
        | Turn c -> c

let getMapChar (carts:Cart list) (row:int) (col:int) (trackPart:TrackPart) : char =
    match List.filter (fun cart -> cart.row = row && cart.col = col) carts with
        | [] -> getTrackChar trackPart
        | [c] -> getCartChar c
        | _ -> 'X'

let buildVisualMap (track:TrackPart[,]) (carts: Cart list)  : char[,] =
    track |> Array2D.mapi (getMapChar carts)

let printGameState (state:GameState) =
    printfn "\n\n-------------------------------------\n"
    let carts = List.concat [state.unmovedCarts; state.movedCarts]
    let map = buildVisualMap state.track carts
    let rows = Array2D.length1 state.track
    let cols = Array2D.length2 state.track

    [0..rows - 1]
        |> List.iter (fun r ->
                            let s =
                                [0..cols-1]
                                    |> List.fold (fun (line:string) (c:int) ->
                                                    let charToAppend = map.[r,c]
                                                    line + (string charToAppend)
                                                 ) ""

                            printfn "%s" s
                        )

let rec solver2 (state:GameState) =
    //printfn "Tick count: %d: Moved: %A Unmoved: %A" state.tickCount state.movedCarts state.unmovedCarts

    match state.unmovedCarts with
        | [] ->
            printfn "Moving to tick count %d" (state.tickCount + 1)
            printGameState state

            solver2 {
                state with
                    unmovedCarts = orderCarts state.movedCarts;
                    movedCarts = [];
                    tickCount = state.tickCount + 1
            }

        | cart :: remainingUnmoved ->
            let trackPart = state.track.[cart.row, cart.col]
            let movedCart = moveCart cart trackPart
            let (collisionDetected, _unmoved, _moved) = filterCollisionsOutOfLists movedCart remainingUnmoved state.movedCarts
            match List.concat [_unmoved; _moved] with
                | [survivor] ->
                    { state with unmovedCarts = _unmoved; movedCarts = _moved }
                | _ ->
                    solver2 {
                        state with
                            unmovedCarts = _unmoved;
                            movedCarts = if collisionDetected then  _moved else movedCart :: _moved
                    }

let solvePartTwo (track:TrackPart[,]) (carts:Cart list) =
    let state = {
        track = track;
        unmovedCarts = carts;
        movedCarts = [];
        tickCount = 0;
        collisions = []
    }

    printGameState state
    let solved = solver2 state
    printfn "%A %A" solved.movedCarts solved.unmovedCarts
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
                                   | '/' -> Turn c
                                   | '\\' -> Turn c
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


    (track, orderCarts cartList)

let solve() =
    // let testdata = Common.getChallengeDataAsArray 2018 13
    // let testdata = Common.getSampleDataAsArray 2018 13
    // getSampleDataFilespecForPart
    let testdata = Common.getSampleDataForPartAsArray 2018 13 2

    // dump "data" testdata

    let (track, carts) = prepareInputData testdata
    //solvePartOne track carts
    solvePartTwo track carts
    ()
