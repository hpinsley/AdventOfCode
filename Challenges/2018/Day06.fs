module Year2018Day06

open System
open System.IO
open Common

type OccupiedCell = int * int

type CellState =
    | Unoccupied
    | Occupied of OccupiedCell

type Area =
    | Finite of int
    | Infinite

let distance (x1, y1) (x2, y2) =
    (abs (x2 - x1)) + (abs (y2 - y1))

let getPoints (inputLine: string) =
    let v = inputLine.Split(",")
                |> Array.map parseInt
    (v.[0], v.[1])

let xCoord (t:int * int): int =
    fst t

let yCoord (t:int * int): int =
    snd t

let getBounds (points: seq<int * int>) =
    let xMax = points |> Seq.map xCoord |> Seq.max
    let yMax = points |> Seq.map yCoord |> Seq.max
    (xMax + 1, yMax + 1)

let setCoordinate (arr: CellState [,]) (x,y) =
    arr.[x, y] <- Occupied <| OccupiedCell (x, y)

let findAbsoluteMinimum (items: ((int * int) * int) list) =
    let minValue = items |> List.map snd |> List.min
    let mins = items
                |> List.filter (fun ((x, y), len) -> len = minValue)
                |> List.map (fun ((x, y), len) -> (x, y))

    match mins with
        | [loneWinner] -> Some loneWinner
        | _ -> None


// Given the entire coordinate space, find the closest.  We are location x,y
// and determining the distances to all occupied cells.  If there is one with
// a nearest distance, it wins.  If there is a tie, there is none.
// each element of grid to see if it an occupied coordinate.  Note that we don't
// actually need to use the coord parameter in this function
let getClosestCoord (occupied: (int * int) list) (x: int) (y: int) (coord: CellState) : OccupiedCell option =

    occupied
        |> List.map (fun (x1, y1) -> ((x1, y1), distance (x1, y1) (x, y)))
        |> findAbsoluteMinimum

// Given an (x,y) coordinate and the closest grid, determine the area
let getArea (closestCoord: ((int * int ) option [,])) (x:int, y:int): Area =
    let rows = Array2D.length1 closestCoord
    let cols = Array2D.length2 closestCoord
    let onEdge (x, y) =
        x = 0 || y = 0 || x = (rows - 1) || y = (cols - 1)
    let closestToCoord =
        closestCoord
            |> Array2D.mapi (fun x y closest -> ((x, y), closest))
            |> Seq.cast<(int * int) * ((int * int) option)>
            |> Seq.choose (fun ((r, c), closest) ->
                                match closest with
                                | Some (x1, y1) ->
                                    if (x1 = x && y1 = y)
                                        then Some (r, c)
                                        else None

                                | None -> None)
            |> List.ofSeq

    if List.exists onEdge closestToCoord
    then
        Infinite
    else
        Finite closestToCoord.Length

let printOccupied (cellState: CellState [,]) =
    let rows = Array2D.length1 cellState
    let cols = Array2D.length2 cellState

    for r in [0..(rows - 1)] do
        printfn ""
        for c in [0..(cols - 1)] do
            printf "%s" <|
                match cellState.[r, c] with
                    | Occupied (x, y) -> (sprintf "[%d,%d]" x y)
                    | Unoccupied -> "[.,.]"

let printClosest (closestCoord: (int * int ) option [,]) =
    let rows = Array2D.length1 closestCoord
    let cols = Array2D.length2 closestCoord

    for r in [0..(rows - 1)] do
        printfn ""
        for c in [0..(cols - 1)] do
            printf "%s" <|
                match closestCoord.[r, c] with
                    | Some (x, y) -> (sprintf "[%d,%d]" x y)
                    | _ -> "[.,.]"

let solvePart1 (state: CellState [,]) (points:(int * int) list) =

    let closestCoord =
        Array2D.mapi (getClosestCoord points) state

    // printOccupied state
    // printfn "\n-------"
    // printClosest closestCoord

    let biggestArea =
        points
            |> List.map (fun (x,y) ->
                            ((x,y), getArea closestCoord (x,y))
                        )
            |> dump "points and area"
            |> List.choose (fun ((x,y), area) ->
                                match area with
                                    | Finite a -> Some ((x,y), a)
                                    | Infinite -> None)
            |> List.maxBy (fun ((x, y), a) -> a)

    printfn "Part (1)\n%A" biggestArea
    ()

let getDistanceToAllPoints (points:(int * int) list) (x:int, y:int) : int =
    points
        |> List.sumBy (fun (coordX, coordY) -> distance (x,y) (coordX, coordY))


let solvePart2 (state: CellState [,]) (maxDist: int) (points:(int * int) list) =

    let rows = Array2D.length1 state
    let cols = Array2D.length2 state

    let closeArea = seq {for r in 0 .. (rows - 1) do for c in 0 .. (cols - 1) -> (r,c)}
                        |> Seq.map (fun (x, y) -> ((x,y), getDistanceToAllPoints points (x,y)))
                        |> Seq.filter (fun (p, dist) -> dist <= maxDist )
                        |> Seq.length


    printfn "\n\nPart (2): %d\n" closeArea
    ()

let solve =
    let testdata = Common.getChallengeDataAsArray 2018 6
    //let testdata = Common.getSampleDataAsArray 2018 6

    let points = testdata |> Array.map getPoints |> List.ofArray
    let bounds = getBounds points
    let state = Array2D.create (xCoord bounds) (yCoord bounds) Unoccupied
    points |>
        List.map (setCoordinate state) |> ignore

    solvePart1 state points
    solvePart2 state 9999 points

    ()
