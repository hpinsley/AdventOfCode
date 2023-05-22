module Common

open System.IO
open System.Text.RegularExpressions

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
