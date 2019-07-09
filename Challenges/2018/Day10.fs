module Year2018Day10

open System
open System.IO
open Common
open System.Text.RegularExpressions

type DimStats =
    {
        count: int;
        min: int;
        max: int;
        range: int;
        stddev: float;
    }
type PointAnalysis =
    {
        rows: DimStats;
        cols: DimStats;
        jointStdDev: float;
    }

let analyzePoints (points:((int * int) * (int * int)) list): PointAnalysis =
    let rows = points |> List.map (fun ((_,y),(_,_)) -> y)
    let rowSet = Set.ofList rows
    let cols = points |> List.map (fun ((x,_),(_,_)) -> x)
    let colSet = Set.ofList cols
    let minRow = Set.minElement rowSet
    let maxRow = Set.maxElement rowSet
    let minCol = Set.minElement colSet
    let maxCol = Set.maxElement colSet
    let numRows = Set.count rowSet
    let numCols = Set.count rowSet
    let rowStd = stdDevList rows
    let colStd = stdDevList cols
    let jointStdDev = rowStd * colStd

    {
        rows = {
            count = numRows;
            min = minRow;
            max = maxRow;
            range = maxRow - minRow;
            stddev = stdDevList rows;
        };
        cols = {
            count = numCols;
            min = minCol;
            max = maxCol;
            range = maxCol - minCol;
            stddev = stdDevList cols
        };

        jointStdDev = jointStdDev;
    }

let plotPoints (points:((int * int) * (int * int)) list) =
    let analysis = analyzePoints points
    let xdim = analysis.cols.range + 1
    let ydim = analysis.rows.range + 1
    let vArray = Array2D.zeroCreate ydim xdim

    points
        |> List.iter (fun ((x,y),(_,_)) ->
                let i = x - analysis.cols.min
                let j = y - analysis.rows.min
                vArray.[j,i] <- 1)

    [0..(Array2D.length1 vArray - 1)]
        |> List.iter (fun r ->
                        // printfn "Printing row %d" r
                        let line =
                            [for c in 0..(Array2D.length2 vArray) - 1 -> vArray.[r,c]]
                                |> List.map (fun v -> if v = 1 then '*' else ' ')
                                |> Array.ofList
                                |> String

                        printfn "%s" line
                     )

    // printfn "\n%A" vArray
    points

let movePointsBy (count:int) (points:((int * int) * (int * int)) list) =
    let moved =
        points
            |> List.map (fun ((x,y),(dx, dy)) -> ((x + count * dx, y + count * dy), (dx, dy)))

    // let stats = analyzePoints moved
    // printfn "\n\nStats:\n%A" stats
    moved

let movePoints = movePointsBy 1

let parseLine (line:string) : ((int * int) * (int * int)) =
    let pattern = "position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+), *(-?\d+)>"
    let matchResult = Regex.Match(line, pattern)
    let x = int matchResult.Groups.[1].Value
    let y = int matchResult.Groups.[2].Value
    let vx = int matchResult.Groups.[3].Value
    let vy = int matchResult.Groups.[4].Value
    ((x,y),(vx,vy))

let rec simulate (priorStdDev:float) (moveCount:int) (points:((int * int) * (int * int)) list) =
    let candidate = movePoints points
    let analysis = analyzePoints candidate
    let newStdDev = analysis.jointStdDev

    if (newStdDev > priorStdDev)
    then
        (points, moveCount)
    else
        // printfn "%A" newStdDev
        simulate newStdDev (moveCount + 1) candidate

let solvePartOne (points:((int * int) * (int * int)) list)  =
    printfn "Starting part one with %d points" points.Length
    points |> analyzePoints |> printfn "%A"
    // let moved =
    //     points
    //         |> movePointsBy 1 |> analyzePoints |> fun a -> a.jointStdDev |> printfn "\n%A"
    //     points
    //         |> movePointsBy 2 |> analyzePoints |> fun a -> a.jointStdDev |> printfn "\n%A"
    //     points
    //         |> movePointsBy 3 |> analyzePoints |> fun a -> a.jointStdDev |> printfn "\n%A"
    //     points
    //         |> movePointsBy 4 |> analyzePoints |> fun a -> a.jointStdDev |> printfn "\n%A"

    //     points |> movePointsBy 3 |> dump "final"

    let (result, moveCount) = simulate (float Single.MaxValue) 0 points
    plotPoints result
    printfn "Result obtained in %d 'moves'" moveCount
    ()

let solvePartTwo  =
    ()

let solve =
    let testdata = Common.getChallengeDataAsArray 2018 10
    //let testdata = Common.getSampleDataAsArray 2018 10
    //dump "data" testdata

    let points = testdata
                    |> List.ofArray
                    |> List.map parseLine

    //dump "parsed" data

    solvePartOne points
    //solvePartTwo
    ()
