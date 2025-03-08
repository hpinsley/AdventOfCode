module Year2022Day11_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked

type WorryFunc = int64 -> int64

type Monkey =
    {
        MonkeyNumber: int64
        ItemList: int64 list
        Operation: string
        Operand: string;
        OperationFunc: WorryFunc
        Test: int64 -> bool
        TrueTarget: int
        FalseTarget: int
        InspectionCount: int64
        Divisor: int64
    }

let makeMonkey (monkeyNumber:int64) : Monkey =
    {
        MonkeyNumber = monkeyNumber
        ItemList = []
        Operation = ""
        Operand = ""
        OperationFunc = fun _ -> 0
        Test = fun _ -> false
        TrueTarget = -1
        FalseTarget = -1
        InspectionCount = 0
        Divisor = 1
    }

let parseOperation (operator:string) (operand:string) (divisor:int64): WorryFunc =
    let fetchOperand = match operand with
                        | "old" -> id
                        | _ as num -> fun _ -> int64 num

    // 96140
    match operator with
        | "*" -> fun old -> ((old) * ((fetchOperand old))) % divisor
        | "+" -> fun old -> ((old) + ((fetchOperand old))) % divisor
        | _ -> failwith "Invalid operator"

let parseLines (lines:string[]) : Monkey[] =
    let mutable monkeyList = []
    let mutable currentMonkey = None

    for line in lines do
        match line with
            | ParseRegex "Monkey (.*):" [monkeyNumber] ->
                if (Option.isSome currentMonkey) then
                    monkeyList <- Option.get currentMonkey :: monkeyList
                currentMonkey <- makeMonkey (int64 monkeyNumber) |> Some
        
            | ParseRegex "Starting items: (.*)" [itemList] ->
                let monkey = Option.get currentMonkey
                currentMonkey <- { monkey with 
                                        ItemList = itemList.Split ", " |> Array.map int64 |> List.ofArray
                                    } |> Some
            | ParseRegex "Operation: new = old (.) (.*)" [operation; operand] ->
                let monkey = Option.get currentMonkey
                currentMonkey <- { monkey with Operation = operation; Operand = operand } |> Some
        
            | ParseRegex "Test: divisible by (.*)" [divisor] ->
                let monkey = Option.get currentMonkey
                let d = int64 divisor
                let f = fun (n:int64) -> n % d = 0

                currentMonkey <- { monkey with 
                                        Test = f;
                                        Divisor = d
                                  } |> Some

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

    let monkeyArray = monkeyList |> List.rev |> Array.ofList
    let productOfDivisors = monkeyArray |> Array.fold (fun p m -> p * m.Divisor) 1L
    let withOperatorFuncs = monkeyArray |> Array.map (fun m -> { m with OperationFunc = parseOperation m.Operation m.Operand productOfDivisors })
    withOperatorFuncs

let playOneRound (postInspectionDivFactor:int64) (monkeys:Monkey[]) : unit =
    for i = 0 to monkeys.Length - 1 do
        let monkey = monkeys[i]
        let toInspect = monkey.ItemList.Length

        for item in monkey.ItemList do
            let worry = (monkey.OperationFunc item) / postInspectionDivFactor
            //printfn $"Monkey {i} worry level is {worry}"

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
    // let lines = Common.getSampleDataAsArray 2022 11
    let lines = Common.getChallengeDataAsArray 2022 11
    //for line in lines do
    //    printfn "%s" line

    let roundsToPlay = 10000

    let worryDivisor = 1

    let monkeys = parseLines lines
//printfn "%A" monkeys

    for i = 1 to roundsToPlay do
        playOneRound worryDivisor monkeys
        
    //for m in monkeys do
    //    printfn "Divisor: %d Monkey %d: Inspection count: %d" worryDivisor m.MonkeyNumber m.InspectionCount

    // printfn "%A" monkeys
    computeMonkeyBusiness monkeys
