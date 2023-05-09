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
        Test: int -> bool
        TrueTarget: int
        FalseTarget: int
        InspectionCount: int
    }

let makeMonkey (monkeyNumber:int) : Monkey =
    {
        MonkeyNumber = monkeyNumber;
        ItemList = [];
        Operation = id;
        Test = fun _ -> false
        TrueTarget = -1;
        FalseTarget = -1;
        InspectionCount = 0;
    }

let parseOperation (operator:string) (operand:string) : WorryFunc =
    let fetchOperand = match operand with
                        | "old" -> id
                        | _ as num -> fun _ -> int num

    match operator with
        | "*" -> fun old -> old * (fetchOperand old)
        | "+" -> fun old -> old + (fetchOperand old)
        | _ -> failwith "Invalid operator"

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
        
            | ParseRegex "Test: divisible by (.*)" [divisor] ->
                let d = int divisor
                let f = fun (n:int) -> n % d = 0

                let monkey = Option.get currentMonkey
                currentMonkey <- { monkey with Test = f } |> Some

            | ParseRegex "If true: throw to monkey (.*)" [m] ->
                let monkey = Option.get currentMonkey
                currentMonkey <- { monkey with TrueTarget = int m } |> Some

            | ParseRegex "If false: throw to monkey (.*)" [m] ->
                let monkey = Option.get currentMonkey
                currentMonkey <- { monkey with FalseTarget = int m } |> Some

            | "" -> ()

            | _ -> failwith "Unknown line"


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