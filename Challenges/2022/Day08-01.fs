module Year2022Day08_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions

type Direction =
    | Left
    | Right
    | Top
    | Bottom

type Tree =
    {
        rowIndex: int;
        colIndex: int;
        height: int;
        mutable visibleFrom: Set<Direction>
        mutable viewableFromTree: Map<Direction, int>;
        mutable score: int;
    }

type CheckType = ByRow | ByCol

type Route =
    {
        direction: Direction;
        checkType: CheckType;
        travelIncrement: int;
    }

type LookInstruction =
    {
        direction:Direction;
        rowDelta: int;
        colDelta: int;
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

let getTreesVisibleFromHomeTree (grid:Tree[,]) (homeTree: Tree) : Map<Direction, int> =
    let tests = [|
        { direction = Left; rowDelta = 0; colDelta = -1 }
        { direction = Right; rowDelta = 0; colDelta = 1 }
        { direction = Top; rowDelta = -1; colDelta = 0 }
        { direction = Bottom; rowDelta = 1; colDelta = 0}
    |]

    let gridSize = Array2D.length1 grid
    let maxIndex = gridSize - 1

    let results = tests
                        |> Array.map (fun test ->
                                        let mutable r = homeTree.rowIndex
                                        let mutable c = homeTree.colIndex
                                        let mutable stop = false
                                        let mutable visible = 0

                                        while (not stop && 
                                                r + test.rowDelta >= 0 && r + test.rowDelta <= maxIndex && 
                                                c + test.colDelta >= 0 && c + test.colDelta <= maxIndex) do
                                            r <- r + test.rowDelta
                                            c <- c + test.colDelta
                                            let testTree = grid[r,c]
                                            visible <- visible + 1

                                            stop <- (testTree.height >= homeTree.height)                                                

                                        (test.direction, visible)
                                        )
                        |> Map.ofArray
    results

let solve =
    // let lines = Common.getSampleDataAsArray 2022 08
    let lines = Common.getChallengeDataAsArray 2022 08
    printfn "%A" lines

    let squareSize = lines[0].Length
    let grid = Array2D.init squareSize squareSize (fun i j 
                                                    -> let c = lines[i][j]
                                                       let height = int c - int '0'
                                                       { rowIndex = i; colIndex = j; height=height; 
                                                            visibleFrom = Set.empty;
                                                            viewableFromTree = Map.empty;
                                                            score = 0;
                                                        })

    // printfn "%A" grid
    printGrid grid (fun (t:Tree) -> t.height.ToString()[0])

    let routes = [| 
        { direction=Left; checkType = ByRow; travelIncrement = 1 }
        { direction=Right; checkType = ByRow; travelIncrement = -1 }
        { direction=Top; checkType = ByCol; travelIncrement = 1 }
        { direction=Bottom; checkType = ByCol; travelIncrement = -1 }
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

    printfn "Part 1: %d trees are visible" visibleCount

    // Compute the view maps for each tree
    grid |> Array2D.iter (fun homeTree -> 
                            let map = getTreesVisibleFromHomeTree grid homeTree
                            homeTree.viewableFromTree <- map
                            if (map.Count > 0)
                            then
                                homeTree.score <- map.Values |> Seq.fold (fun product viewCount -> product * viewCount) 1 
                         )

    //printfn "%A" grid

    //printGrid grid (fun t -> t.height.ToString()[0])


    let mutable bestTree = grid[0,0]
    grid |> Array2D.iter (fun t -> if (t.score > bestTree.score) then bestTree <- t)

    printfn "Best tree (part 2): %A" bestTree