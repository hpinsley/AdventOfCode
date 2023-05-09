module Year2022Day11_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions

type WorryFunc = int -> int

type Monkey =
    {
        MonkeyNumber: int
        ItemList: int list
        Operation: WorryFunc
        Test: int -> int
        TrueTarget: int
        FalseTarget: int
        InspectionCount: int
    }

let makeMonkey (monkeyNumber:int) : Monkey =
    {
        MonkeyNumber = monkeyNumber;
        ItemList = [];
        Operation = id;
        Test = id
        TrueTarget = -1;
        FalseTarget = -1;
        InspectionCount = 0;
    }

let parseOperation (operator:string) (operand:string) : WorryFunc =
    fun old -> old * old

let parseLines (lines:string[]) : Monkey[] =
    let mutable monkeyList = []
    let mutable currentMonkey = None

    for line in lines do
        match line with
            | ParseRegex "Monkey (.*):" [monkeyNumber] ->
                if (Option.isSome currentMonkey) then
                    monkeyList <- Option.get currentMonkey :: monkeyList
                currentMonkey <- makeMonkey (int monkeyNumber) |> Some
        
            | ParseRegex "Starting items: (.*)" [itemList] ->
                let monkey = Option.get currentMonkey
                currentMonkey <- { monkey with 
                                        ItemList = itemList.Split ", " |> Array.map int |> List.ofArray
                                    } |> Some
            | ParseRegex "Operation: new = old (.) (.*)" [operation; operand] ->
                let operation = parseOperation operation operand

                let monkey = Option.get currentMonkey
                currentMonkey <- { monkey with Operation = operation } |> Some
        
            | _ -> ()


    if (Option.isSome currentMonkey) then
        monkeyList <- Option.get currentMonkey :: monkeyList

    monkeyList |> List.rev |> Array.ofList

let solve =
    let lines = Common.getSampleDataAsArray 2022 11
    // let lines = Common.getChallengeDataAsArray 2022 11
    for line in lines do
        printfn "%s" line

    let monkeys = parseLines lines
    printfn "%A" monkeys