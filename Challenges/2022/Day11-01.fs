module Year2022Day11_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked

type WorryFunc = int64 -> int64

type Monkey =
    {
        MonkeyNumber: int
        ItemList: int64 list
        Operation: WorryFunc
        Test: int64 -> bool
        TrueTarget: int
        FalseTarget: int
        InspectionCount: int64
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
                        | _ as num -> fun _ -> int64 num

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
                                        ItemList = itemList.Split ", " |> Array.map int64 |> List.ofArray
                                    } |> Some
            | ParseRegex "Operation: new = old (.) (.*)" [operation; operand] ->
                let operation = parseOperation operation operand

                let monkey = Option.get currentMonkey
                currentMonkey <- { monkey with Operation = operation } |> Some
        
            | ParseRegex "Test: divisible by (.*)" [divisor] ->
                let d = int64 divisor
                let f = fun (n:int64) -> n % d = 0

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

let playOneRound (postInspectionDivFactor:int64) (monkeys:Monkey[]) : unit =
    for i = 0 to monkeys.Length - 1 do
        let monkey = monkeys[i]
        let toInspect = monkey.ItemList.Length

        for item in monkey.ItemList do
            let worry = (monkey.Operation item) / postInspectionDivFactor
            printfn $"Monkey {i} worry level is {worry}"

            let target = if (monkey.Test worry) then monkey.TrueTarget else monkey.FalseTarget
            let targetMonkey = monkeys[target]
            monkeys[target] <- { targetMonkey with ItemList = List.append targetMonkey.ItemList [worry] }
        
        monkeys[i] <- { monkey with 
                            InspectionCount = monkey.InspectionCount + int64 toInspect;
                            ItemList = []
                      } 

let computeMonkeyBusiness (monkeys:Monkey[]) : unit =
        let monkeyBusiness = 
                monkeys
                    |> Array.sortByDescending (fun m -> m.InspectionCount)
                    |> Array.take 2
                    |> Array.fold (fun product monkey -> product * monkey.InspectionCount) 1L 

        printfn "MonkeyBusiness = %d" monkeyBusiness    

let solve =
    let lines = Common.getSampleDataAsArray 2022 11
    // let lines = Common.getChallengeDataAsArray 2022 11
    //for line in lines do
    //    printfn "%s" line

    let maxWorryDivisor = 1
    let roundsToPlay = 20

    for worryDivisor = 1 to maxWorryDivisor do
        printfn ""

        let monkeys = parseLines lines
    //printfn "%A" monkeys

        for i = 1 to roundsToPlay do
            playOneRound worryDivisor monkeys
        
        for m in monkeys do
            printfn "Divisor: %d Monkey %d: Inspection count: %d" worryDivisor m.MonkeyNumber m.InspectionCount

        // printfn "%A" monkeys
        // computeMonkeyBusiness monkeys
