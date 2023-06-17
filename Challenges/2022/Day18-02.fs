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

    if (not onlyDifferentOccludeds || cube1.cubeType <> cube2.cubeType) then
        match getCommonSide cube1 cube2 with
                | Some (side1, side2) ->
                    //printfn "Cube %d side %A matches Cube %d side %A"
                    //            cube1.cubeNumber side1.sideName
                    //            cube2.cubeNumber side2.sideName
   
                    cube1.occludedCount <- cube1.occludedCount + 1
                    cube2.occludedCount <- cube2.occludedCount + 1
                | None ->
                    ()


let findSurfaceArea (droplets:Cube[]) : int =

    let coordsX = droplets|> Seq.map (fun c -> c.allPoints |> Seq.map (fun p -> p.x)) |> Seq.concat
    let coordsY = droplets|> Seq.map (fun c -> c.allPoints |> Seq.map (fun p -> p.y)) |> Seq.concat
    let coordsZ = droplets|> Seq.map (fun c -> c.allPoints |> Seq.map (fun p -> p.z)) |> Seq.concat

    let minX = (Seq.min coordsX) - 1
    let minY = (Seq.min coordsY) - 1
    let minZ = (Seq.min coordsZ) - 1
    let maxX = (Seq.max coordsX) + 1
    let maxY = (Seq.max coordsY) + 1
    let maxZ = (Seq.max coordsZ) + 1

    let exteriorOrigins = droplets
                            |> Seq.map (fun c -> c.origin)
                            |> Set.ofSeq
    
    let exteriorCount = exteriorOrigins.Count
    let startingPoint = { x = minX; y = minY; z = minZ }
    let airOrigins = HashSet<Point>()

    let queue = Queue<Point>()
    queue.Enqueue startingPoint
    while (queue.Count > 0) do
        let p = queue.Dequeue()

        if (
               (p.x >= minX && p.x <= maxX)
            && (p.y >= minY && p.y <= maxY)
            && (p.z >= minZ && p.z <= maxZ)
            && not (Set.contains p exteriorOrigins)
            && not (airOrigins.Contains(p))
        )
        then
            airOrigins.Add(p) |> ignore

            queue.Enqueue({ p with x = p.x - 1 })
            queue.Enqueue({ p with x = p.x + 1 })
            queue.Enqueue({ p with y = p.y - 1 })
            queue.Enqueue({ p with y = p.y + 1 })
            queue.Enqueue({ p with z = p.z - 1 })
            queue.Enqueue({ p with z = p.z + 1 })


    let airCubes = airOrigins
                    |> Seq.mapi (fun i origin -> generateCube Interior (i + exteriorCount) origin)
                    |> Array.ofSeq

    // Now that we've filled all possible air
    let surfaceArea = airCubes |> Array.fold 
                                    (fun (hits:int) (air:Cube) -> 
                                            let hitsForThisPocket = 
                                                droplets
                                                    |> Array.sumBy(
                                                            fun droplet ->
                                                                match getCommonSide droplet air with
                                                                    | Some (_, _) -> 1
                                                                    | None -> 0
                                                                  )
                                            hits + hitsForThisPocket
                                    )
                                    0

    surfaceArea
 
let solve =
    // let lines = Common.getSampleDataAsArray 2022 18
    let lines = Common.getChallengeDataAsArray 2022 18


    // printAllLines lines
    let originPoints = getOriginPoints lines
    printfn "There are %d cubes to build" originPoints.Length
    printfn ""

    let exteriorCubes = originPoints |> Array.mapi (generateCube Exterior)
    let surfaceArea = findSurfaceArea exteriorCubes
    printfn "%d" surfaceArea

    ()
