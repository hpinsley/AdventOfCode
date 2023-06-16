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

let compareAndMarkCubes (onlyDifferentOccludeds:bool) (cube1:Cube) (cube2:Cube) : unit =
    // printfn "Comparing cube %d to cube %d" cube1.cubeNumber cube2.cubeNumber
    let targetCubeOrigin = { x = 2; y = 1; z = 5 }

    if (not onlyDifferentOccludeds || cube1.cubeType <> cube2.cubeType) then
        match getCommonSide cube1 cube2 with
                | Some (side1, side2) ->
                    //printfn "Cube %d side %A matches Cube %d side %A"
                    //            cube1.cubeNumber side1.sideName
                    //            cube2.cubeNumber side2.sideName

                    if (cube1.origin = targetCubeOrigin || cube2.origin = targetCubeOrigin)
                    then
                        printfn "%b Occlusion between (%d,%d,%d) and (%d,%d,%d)"
                                    onlyDifferentOccludeds
                                    cube1.origin.x cube1.origin.y cube1.origin.z
                                    cube2.origin.x cube2.origin.y cube2.origin.z

                    cube1.occludedCount <- cube1.occludedCount + 1
                    cube2.occludedCount <- cube2.occludedCount + 1
                | None ->
                    ()

let findFillerCubes (droplets:Cube[]) : Cube[] =

    let coordsX = droplets|> Seq.map (fun c -> c.allPoints |> Seq.map (fun p -> p.x)) |> Seq.concat
    let coordsY = droplets|> Seq.map (fun c -> c.allPoints |> Seq.map (fun p -> p.y)) |> Seq.concat
    let coordsZ = droplets|> Seq.map (fun c -> c.allPoints |> Seq.map (fun p -> p.z)) |> Seq.concat

    let minX = Seq.min coordsX
    let minY = Seq.min coordsY
    let minZ = Seq.min coordsZ
    let maxX = Seq.max coordsX
    let maxY = Seq.max coordsY
    let maxZ = Seq.max coordsZ

    let exteriorOrigins = droplets
                            |> Seq.map (fun c -> c.origin)
                            |> Set.ofSeq

    let exteriorCount = exteriorOrigins.Count

    let interiorCubes = 
        seq {
            for x in seq {minX .. maxX } do
                for y in seq {minY .. maxY } do
                    for z in seq { minZ .. maxZ } do
                        let interiorOrigin = { x = x; y = y; z = z }
                        if (not (Set.contains interiorOrigin exteriorOrigins))
                        then
                            if (
                                     (Seq.exists (fun p -> p.x = interiorOrigin.x && p.y = interiorOrigin.y && p.z < interiorOrigin.z) exteriorOrigins)
                                  && (Seq.exists (fun p -> p.x = interiorOrigin.x && p.y = interiorOrigin.y && p.z > interiorOrigin.z) exteriorOrigins)
                                  && (Seq.exists (fun p -> p.y = interiorOrigin.y && p.z = interiorOrigin.z && p.x < interiorOrigin.x) exteriorOrigins)
                                  && (Seq.exists (fun p -> p.y = interiorOrigin.y && p.z = interiorOrigin.z && p.x > interiorOrigin.x) exteriorOrigins)
                                  && (Seq.exists (fun p -> p.z = interiorOrigin.z && p.x = interiorOrigin.x && p.y < interiorOrigin.y) exteriorOrigins)
                                  && (Seq.exists (fun p -> p.z = interiorOrigin.z && p.x = interiorOrigin.x && p.y > interiorOrigin.y) exteriorOrigins)
                            )
                            then
                                yield interiorOrigin
        } 
            |> Seq.mapi (fun i origin -> generateCube Interior (i + exteriorCount) origin)
            |> Array.ofSeq

    interiorCubes
 
let solve =
    // let lines = Common.getSampleDataAsArray 2022 18
    let lines = Common.getChallengeDataAsArray 2022 18
    // let lines = [| "1,1,1"; "2,1,1"|]
    // printAllLines lines
    let originPoints = getOriginPoints lines
    printfn "There are %d cubes to build" originPoints.Length
    printfn ""

    let exteriorCubes = originPoints |> Array.mapi (generateCube Exterior)
    let interiorCubes = findFillerCubes exteriorCubes
    
    //for (cube1, cube2) in allCombinations exteriorCubes do
    //    compareAndMarkCubes false cube1 cube2

    //for (cube1, cube2) in allCombinations interiorCubes do
    //    compareAndMarkCubes false cube1 cube2

    let allCubes = Array.append exteriorCubes interiorCubes
    for (cube1, cube2) in allCombinations allCubes do
        compareAndMarkCubes false cube1 cube2

    // 2070 is too low
    let sideCount = 6 * exteriorCubes.Length
    let occludedCount = exteriorCubes |> Array.sumBy (fun c -> c.occludedCount)
    let viewAbleCount = sideCount - occludedCount

    let interiorSideCount = 6 * interiorCubes.Length
    let interiorOccludedCount = interiorCubes |> Array.sumBy (fun c -> c.occludedCount)
    let interiorViewableCount = interiorSideCount - interiorOccludedCount

    printfn "Exterior: Of the %d cubes with %d sides, %d are occluded and %d are visible"
        exteriorCubes.Length sideCount occludedCount viewAbleCount
    printfn "Interior: Of the %d cubes with %d sides, %d are occluded and %d are visible"
        interiorCubes.Length interiorSideCount interiorOccludedCount interiorViewableCount
    
    let surfaceArea = viewAbleCount - interiorViewableCount
    printfn "Surface area is %d" surfaceArea
    ()
