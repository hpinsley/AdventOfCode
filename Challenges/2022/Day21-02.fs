module Year2022Day21_Part2

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type Opcode =
    | ADD
    | SUB
    | DIV
    | MUL

type OpMonkeyDef =
    {
        opCode: Opcode
        m1: string
        m2: string
    }

type MonkeyAction =
    | Number of decimal
    | Operation of OpMonkeyDef

type Monkey =
    {
        name: string
        action: MonkeyAction
    }

let monkeyDict = new Dictionary<string, unit -> decimal>()


let buildLookups (monkeys:Monkey[]) : unit =
    let lookupMonkeyResolver name =
        monkeyDict[name]

    let monkeysAndResolvers =
        monkeys
            |> Array.map (fun m -> 
                            match m.action with
                                | Number n -> (m.name, fun () -> n)
                                | Operation opDef ->

                                    let op =
                                        match opDef.opCode with
                                        | Opcode.ADD -> fun () -> 
                                                (lookupMonkeyResolver opDef.m1)() 
                                              + (lookupMonkeyResolver opDef.m2)()
                                        | Opcode.SUB -> fun () -> 
                                                (lookupMonkeyResolver opDef.m1)() 
                                              - (lookupMonkeyResolver opDef.m2)()
                                        | Opcode.MUL -> fun () -> 
                                                (lookupMonkeyResolver opDef.m1)() 
                                              * (lookupMonkeyResolver opDef.m2)()
                                        | Opcode.DIV -> fun () -> 
                                                (lookupMonkeyResolver opDef.m1)() 
                                              / (lookupMonkeyResolver opDef.m2)()
                                    
                                    (m.name, op)
                          )
    for (name, resolver) in monkeysAndResolvers do
        monkeyDict.Add(name, resolver)

let parseLine (line:string) : Monkey =
    let pattern1 = "(....): (....) ([+\-*/]) (....)"
    let pattern2 = "(....): ([0-9]+)"

    let match1 = Regex.Match(line, pattern1)
    let match2 = Regex.Match(line, pattern2)

    if (not (match1.Success || match2.Success))
    then
        failwith "no match"

    if (match1.Success && match2.Success)
    then
        failwith "Both match?"

    if (match1.Success)
    then
        let name = match1.Groups.[1].Value
        let m1 = match1.Groups.[2].Value
        let opCode = match1.Groups.[3].Value
        let m2 = match1.Groups.[4].Value

        let opCode =
            if (name = "root")
            then
                Opcode.SUB  // Change the root opertion to subtraction to make equality test against 0
            else
                match opCode with
                | "+" -> Opcode.ADD
                | "-" -> Opcode.SUB
                | "*" -> Opcode.MUL
                | "/" -> Opcode.DIV
                | _ -> failwith "unknown opcode"

        let opDef =
            {
                opCode = opCode
                m1 = m1
                m2 = m2
            }

        let action = Operation opDef

        {
            name = name
            action = action
        }
    else
        let name = match2.Groups.[1].Value
        let number = match2.Groups.[2].Value |> int

        let action = Number number

        {
            name = name
            action = action
        }

let parseLines (lines:string[]) : Monkey[] =
    lines
        |> Array.map parseLine

let getHumanDrivenResult (root:unit -> decimal) (humanValue:decimal) : decimal =
    monkeyDict["humn"] <- fun () -> humanValue
    let result = root()
    result

let solvePart2() =
    let root = monkeyDict["root"]
    let left =  -4000000000000.0M
    let right =  4000000000000.0M
    let mid = 0M

    let rec bisect (left:decimal) (right:decimal) (mid:decimal) : decimal =
        let result = getHumanDrivenResult root mid
        printfn "(%A - %A) %A\t%A" left right mid result
        if (result = 0M)
        then
            mid
        elif (result < 0M)  // You don't know how changes to the humn value will affect the result.
        // It appears the sample affects the result in one direction but the challenge data does not.
        then
            bisect left mid ((left + mid) / 2.0M)
        else
            bisect mid right ((mid + right) / 2.0M)
    
    let result = bisect left right mid        
    result

    //while true do
    //    printf "Enter human value: "
    //    let input = Console.ReadLine()
    //    let v = decimal.Parse input
    //    let result = getHumanDrivenResult root v
    //    printfn "%A" result

let solve =
    // let lines = Common.getSampleDataAsArray 2022 21
    let lines = Common.getChallengeDataAsArray 2022 21
    //printAllLines lines

    let parsed = parseLines lines
    //printfn "%A" parsed
    buildLookups parsed

    let result = solvePart2()

    //let root = monkeyDict["root"]
    //let node1 = monkeyDict["pvgq"]
    //let node2 = monkeyDict["ngpl"]

    //// Got this with the bisect method.  It yielded 3441198826074L, but there must be an integer
    //// division that gives multiple correct answers.  I tried 3441198826073L and it worked too but
    //// the accepted answer is 3441198826073L
    //let humanValue = 3441198826073L
    //monkeyDict["humn"] <- fun () -> humanValue
    //let v1 = node1()
    //let v2 = node2()
    //printfn "%d:\t%d\t%d" humanValue v1 v2

    //monkeyDict["humn"] <- fun () -> humanValue
    //let result = getHumanDrivenResult root 3441198826074L
    
    printfn "%A" result
    ()
