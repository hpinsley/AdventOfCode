module Year2022Day18_Part2

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Point = {
    x: int
    y: int
    z: int
}

type SideName =
    | Bottom
    | Top
    | Left
    | Right
    | Front
    | Back

type NamedSide =
    {
        sideName: SideName
        points: Set<Point>
    }

type CubeType =
    | Exterior
    | Interior

type Cube = {
    cubeNumber: int
    cubeType: CubeType
    origin: Point
    bottom: NamedSide
    top: NamedSide
    left: NamedSide
    right: NamedSide
    front: NamedSide
    back: NamedSide
    allPoints: Set<Point>

    mutable occludedCount: int
}

let generateCube (cubeType:CubeType) (cubeNumber:int) (origin:Point): Cube =
    let bottom  = [|    { x = origin.x; y = origin.y; z = origin.z }; 
                    { x = origin.x; y = origin.y + 1; z = origin.z };
                    { x = origin.x + 1; y = origin.y; z = origin.z };
                    { x = origin.x + 1; y = origin.y + 1; z = origin.z }
                    |] |> Set.ofArray

    let top  = [|       { x = origin.x; y = origin.y; z = origin.z + 1}; 
                    { x = origin.x; y = origin.y + 1; z = origin.z + 1 };
                    { x = origin.x + 1; y = origin.y; z = origin.z + 1};
                    { x = origin.x + 1; y = origin.y + 1; z = origin.z + 1 }
                    |] |> Set.ofArray

    let left  = [|      { x = origin.x; y = origin.y; z = origin.z}; 
                    { x = origin.x; y = origin.y + 1; z = origin.z };
                    { x = origin.x; y = origin.y; z = origin.z + 1};
                    { x = origin.x; y = origin.y + 1; z = origin.z + 1 }
                    |] |> Set.ofArray
    let right  = [|      { x = origin.x + 1; y = origin.y; z = origin.z}; 
                    { x = origin.x + 1; y = origin.y + 1; z = origin.z };
                    { x = origin.x + 1; y = origin.y; z = origin.z + 1};
                    { x = origin.x + 1; y = origin.y + 1; z = origin.z + 1 }
                    |] |> Set.ofArray

    let back  = [|      { x = origin.x; y = origin.y + 1; z = origin.z}; 
                    { x = origin.x + 1; y = origin.y + 1; z = origin.z };
                    { x = origin.x; y = origin.y + 1; z = origin.z };
                    { x = origin.x + 1; y = origin.y + 1; z = origin.z + 1 }
                    |] |> Set.ofArray
    let front  = [|     { x = origin.x; y = origin.y; z = origin.z}; 
                    { x = origin.x + 1; y = origin.y; z = origin.z };
                    { x = origin.x; y = origin.y; z = origin.z };
                    { x = origin.x + 1; y = origin.y; z = origin.z + 1 }
                    |] |> Set.ofArray

    let (cube:Cube) = {
                        cubeNumber = cubeNumber
                        cubeType = cubeType
                        origin = origin
                        occludedCount = 0
                        bottom = { sideName = Bottom; points = bottom }
                        top = { sideName = Top; points = top }
                        left = { sideName = Left; points = left }
                        right = { sideName = Right; points = right }
                        front = { sideName = Front; points = front }
                        back = { sideName = Back; points = back }
                        allPoints = bottom
                                        |> Set.union top
                                        |> Set.union bottom
                                        |> Set.union left
                                        |> Set.union right
                                        |> Set.union front
                                        |> Set.union back

                      }

    cube

let getCommonSide (cube1:Cube) (cube2:Cube): (NamedSide * NamedSide) option =
    let comparisons = seq {
        (cube1.left, cube2.right); (cube1.right, cube2.left)
        (cube1.top, cube2.bottom); (cube1.bottom, cube2.top)
        (cube1.front, cube2.back); (cube1.back, cube2.front)
    }
    
    comparisons |> Seq.tryFind (fun (side1, side2) -> side1.points = side2.points)
    

let getOriginPoints (lines:string[]) : Point[] =
    lines |>
        Array.map (fun s -> 
                    let split = s.Split(',')
                    {
                        x = int split[0]
                        y = int split[1]
                        z = int split[2]
                    }
                )

let compareAndMarkCubes (cube1:Cube) (cube2:Cube) : unit =
    // printfn "Comparing cube %d to cube %d" cube1.cubeNumber cube2.cubeNumber
    match getCommonSide cube1 cube2 with
            | Some (side1, side2) ->
                printfn "Cube %d side %A matches Cube %d side %A"
                            cube1.cubeNumber side1.sideName
                            cube2.cubeNumber side2.sideName

                cube1.occludedCount <- cube1.occludedCount + 1
                cube2.occludedCount <- cube2.occludedCount + 1
            | None ->
                ()
let findInteriorCubes (droplets:Cube[]) : Cube[] =

    let coordsX = droplets|> Seq.map (fun c -> c.allPoints |> Seq.map (fun p -> p.x)) |> Seq.concat
    let coordsY = droplets|> Seq.map (fun c -> c.allPoints |> Seq.map (fun p -> p.y)) |> Seq.concat
    let coordsZ = droplets|> Seq.map (fun c -> c.allPoints |> Seq.map (fun p -> p.z)) |> Seq.concat

    let minX = Seq.min coordsX
    let minY = Seq.min coordsY
    let minZ = Seq.min coordsZ
    let maxX = Seq.max coordsX
    let maxY = Seq.max coordsY
    let maxZ = Seq.max coordsZ
    printfn "Extent is from (%d, %d, %d) to (%d, %d, %d)" minX minY minZ maxX maxY maxZ
    [||]

let solve =
    let lines = Common.getSampleDataAsArray 2022 18
    // let lines = Common.getChallengeDataAsArray 2022 18
    // let lines = [| "1,1,1"; "2,1,1"|]
    // printAllLines lines
    let originPoints = getOriginPoints lines
    printfn "There are %d cubes to build" originPoints.Length
    printfn ""

    let cubes = originPoints |> Array.mapi (generateCube Exterior)

    let interiorCubes = findInteriorCubes cubes

    for (cube1, cube2) in allCombinations cubes do
        compareAndMarkCubes cube1 cube2

    let sideCount = 6 * cubes.Length
    let occludedCount = cubes |> Array.sumBy (fun c -> c.occludedCount)
    let viewAbleCount = sideCount - occludedCount

    printfn "Of the %d sides, %d are occluded and %d are visible"
        sideCount occludedCount viewAbleCount
    ()
