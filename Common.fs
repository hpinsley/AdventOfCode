module Common

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

let bold = fun text -> $"\x1b[1m{text}\x1b[0m"
let bold_red = fun text -> $"\x1b[1;31m{text}\x1b[0m"
let bold_green = fun text -> $"\x1b[1;32m{text}\x1b[0m"
let bold_yellow = fun text -> $"\x1b[1;33m{text}\x1b[0m"

let dump label o =
    printfn "\n%s:\n%A" label o
    o

let getSampleDataFilespec year month =
    sprintf "InputFiles/Sample-%04d-%02d.txt" year month

let getSampleDataFilespecForPart year month part =
    sprintf "InputFiles/Sample-%04d-%02d-part%02d.txt" year month part

let getChallengeDataFilespec year month =
    sprintf "InputFiles/input-%04d-%02d.txt" year month

let getChallengeData year month: string =
    let file = getChallengeDataFilespec year month
    printf "Reading from file %s\n" file
    File.ReadAllText file

let getChallengeDataAsArray year month: string [] =
    let file = getChallengeDataFilespec year month
    printf "Reading from file %s\n" file
    File.ReadAllLines file

let getSampleData year month: string =
    let file = getSampleDataFilespec year month
    printf "Reading from file %s\n" file
    File.ReadAllText file

let getSampleDataAsArray year month : string [] =
    let file = getSampleDataFilespec year month
    printf "Reading from file %s\n" file
    File.ReadAllLines file

let getSampleDataForPartAsArray year month part : string [] =
    let file = getSampleDataFilespecForPart year month part
    printf "Reading from file %s\n" file
    File.ReadAllLines file

let trim (s:string) : string = s.Trim()
let parseInt = trim >> int
let inline tuple2 a b = a,b

// The following example computes the average of a list.
let averageList list = (List.fold (fun acc elem -> acc + float elem) 0.0 list / float list.Length)

// The following example computes the standard deviation of a list.
// The standard deviation is computed by taking the square root of the
// sum of the variances, which are the differences between each value
// and the average.
let stdDevList list =
    let avg = averageList list
    sqrt (List.fold (fun acc elem -> acc + (float elem - avg) ** 2.0 ) 0.0 list / float list.Length)

let printGrid (grid:'T[,]) (cellToCharFunc: 'T -> char) =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid
    let r0 = Array2D.base1 grid
    let c0 = Array2D.base2 grid

    [r0..r0 + rows - 1]
        |> List.iter (fun r ->
                            let s =
                                [c0..c0 + cols-1]
                                    |> List.fold (fun (line:string) (c:int) ->
                                                    let charToAppend = cellToCharFunc grid.[r,c]
                                                    line + (string charToAppend)
                                                 ) ""

                            printfn "%s" s
                        )

// ParseRegex parses a regular expression and returns a list of the strings that match each group in
// the regular expression.
// List.tail is called to eliminate the first element in the list, which is the full matched expression,
// since only the matches for each group are wanted.
// This is an Active Pattern
let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let printAllLines (lines:seq<string>) : unit =
    for line in lines do
        printfn "%s" line

// Given a positive integer n, return all combinations of indices split between two "consumers"
let allSplits (n: int) : (int list * int list) list =
    let getOnBits (n: int) : seq<int> =
        
        seq {
            let mutable v = n
            let mutable bitNumber = 0

            while v <> 0 do
                if (v &&& 1 = 1) then yield bitNumber
                v <- v >>> 1
                bitNumber <- bitNumber + 1
        }
    
    seq {
        let truncationMask = (pown 2 n) - 1
        for k in seq { 0 .. truncationMask } do
            let mask1 = k
            let mask2 = ~~~k &&& truncationMask
            let onBits1 = getOnBits mask1 |> List.ofSeq
            let onBits2 = getOnBits mask2 |> List.ofSeq
            yield (onBits1, onBits2)
    } |> List.ofSeq
 
let allCombinations<'a> (v:'a[]) : ('a * 'a) seq =
    seq {
        for i in seq {0..v.Length - 1} do
            for j in seq {i + 1 .. v.Length - 1} do
                yield (v[i], v[j])
    }

// I have not tested this algorithm (Floyd's cycle-finding algorith)
let findCycle (str: string) : (int * int) =
    let tortoise = str[0]
    let hare = str[0]

    let rec findMeetingPoint (t: char) (h: char) =
        if t = h then t
        else findMeetingPoint str[int(t)] str[int(str[int(h)])]

    let cycleStart = str[0]
    let meetingPoint = findMeetingPoint tortoise hare

    let rec findCycleStart (cs: char) (mp: char) =
        if cs = mp then cs
        else findCycleStart str[int(cs)] str[int(mp)]

    let rec findCycleLength (cs: char) (cl: int) (mp: char) =
        if cs <> mp then findCycleLength str[int(cs)] (cl + 1) str[int(mp)]
        else cl

    let cycleStartPoint = findCycleStart cycleStart meetingPoint
    let cycleLength = findCycleLength cycleStartPoint 1 cycleStartPoint

    (int(cycleStartPoint)), cycleLength

let aStar 
    (start:'T)                      // The starting node 
    (isGoal:'T -> bool)             // Made this a function in case the goal moves
    (getNeighbers:'T -> 'T list)
    (h:'T -> int)                   // Heuristic function
                    : unit =
    
    let gscore = new Dictionary<'T, int>()
    
    let getGscore (node:'T) : int =
        let (found, valFound) = gscore.TryGetValue(node)
        if found then valFound else Int32.MaxValue

    let hscore = new Dictionary<'T, int>()
    
    let getHscore (node:'T) : int =
        let (found, valFound) = hscore.TryGetValue(node)
        if found then valFound else Int32.MaxValue

    let cameFrom = new Dictionary<'T,'T>()
    let reconstruct_path (current:'T) : 'T list =
        HERE

    total_path := {current}
    while current in cameFrom.Keys:
        current := cameFrom[current]
        total_path.prepend(current)
    return total_path

    let openSet = new PriorityQueue<'T, int>()
    openSet.Enqueue (start, Int32.MaxValue)

    while (openSet.Count > 0) do
        let current = openSet.Dequeue()


    ()
