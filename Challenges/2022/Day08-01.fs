module Year2022Day08_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions

type Direction =
    | FromLeft
    | FromRight
    | FromTop
    | FromBottom

type Tree =
    {
        rowIndex: int;
        colIndex: int;
        height: int;
        mutable visibleFrom: Set<Direction>
    }

type CheckType = ByRow | ByCol

type Route =
    {
        direction: Direction;
        checkType: CheckType;
        travelIncrement: int;
    }

let getTreeLine (grid:Tree[,]) (route:Route) : seq<seq<Tree>> =
    let gridSize = Array2D.length1 grid
    let maxIndex = gridSize - 1

    match route.checkType with
        | ByRow ->
            let rowSeq = {0..maxIndex}
            let colSeq = if route.travelIncrement > 0 then {0 .. maxIndex} else {maxIndex .. -1 .. 0}
            seq { for r in rowSeq do
                            let s = colSeq |> Seq.map (fun c -> grid[r,c])
                            yield s
                 }
        | ByCol ->
            let colSeq = {0..maxIndex}
            let rowSeq = if route.travelIncrement > 0 then {0 .. maxIndex} else {maxIndex .. -1 .. 0}
            seq { for c in colSeq do
                            let s = rowSeq |> Seq.map (fun r -> grid[r,c])
                            yield s
                 }

let marktTreeVisibleFrom (tree:Tree) (direction:Direction) : unit =
        tree.visibleFrom <- Set.add direction tree.visibleFrom

let treeVisibleFrom (tree:Tree) (direction:Direction): bool =
    Set.contains direction tree.visibleFrom

let solve =
    // let lines = Common.getSampleDataAsArray 2022 08
    let lines = Common.getChallengeDataAsArray 2022 08
    printfn "%A" lines

    let squareSize = lines[0].Length
    let grid = Array2D.init squareSize squareSize (fun i j 
                                                    -> let c = lines[i][j]
                                                       let height = int c - int '0'
                                                       { rowIndex = i; colIndex = j; height=height; visibleFrom = Set.empty })

    // printfn "%A" grid
    printGrid grid (fun (t:Tree) -> t.height.ToString()[0])

    let routes = [| 
        { direction=FromLeft; checkType = ByRow; travelIncrement = 1 }
        { direction=FromRight; checkType = ByRow; travelIncrement = -1 }
        { direction=FromTop; checkType = ByCol; travelIncrement = 1 }
        { direction=FromBottom; checkType = ByCol; travelIncrement = -1 }
    |]
    
    for route in routes do
        let linesToCheck = getTreeLine grid route

        for lineToCheck in linesToCheck do
            let mutable maxHeight = -1

            for tree in lineToCheck do
                if (tree.height > maxHeight)
                then
                    marktTreeVisibleFrom tree route.direction
                    maxHeight <- tree.height
                else ()

    printGrid grid (fun t -> if Set.isEmpty t.visibleFrom then ' ' else 'T')

    let mutable visibleCount = 0
    Array2D.iter (fun (t:Tree) -> if (not (Set.isEmpty t.visibleFrom))
                                  then visibleCount <- visibleCount + 1
                                  else ()
                    ) grid

    printfn "%d trees are visible" visibleCount