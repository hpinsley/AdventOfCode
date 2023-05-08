module Year2022Day10_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions

type Register = {
    value: int
}

type Cycles = int

type Cpu = {
    xRegister: Register
    clock: int
}

type State = {
    cpuStack: Cpu list
}

type CpuOperation = Cpu -> Cpu

type Instruction =
    | NoOp of CpuOperation
    | AddToXRegister of int * CpuOperation

let cpu = {
    xRegister = { value = 1 }
    clock = 0
}

let lineToInstruction (line:string) : Instruction =
    match line with
        | Common.ParseRegex "noop" [] -> NoOp (fun cpu -> { cpu with clock = cpu.clock + 1 })
        | Common.ParseRegex "addx (.*)" [value] -> AddToXRegister (
                                                    int value, 
                                                    fun cpu -> { 
                                                                    cpu with 
                                                                        xRegister = { cpu.xRegister with value = cpu.xRegister.value + int value }
                                                                        clock = cpu.clock + 2 
                                                                })

let processInstruction (state:State) (instruction:Instruction) : State =
    let cpu = List.head state.cpuStack

    //printfn "Processing %A against %A" instruction cpu

    let nextCpu = match instruction with
                    | NoOp cpuFunc -> cpuFunc cpu
                    | AddToXRegister (_, cpuFunc) -> cpuFunc cpu
    { state with cpuStack = nextCpu :: state.cpuStack }

let rec AddMissingClockStates (filledIn:Cpu list) (hasGaps:Cpu list): Cpu list =
    match hasGaps with
        | [] -> filledIn
        | cpu::tail -> 
            let cpusToAppend = match List.tryLast filledIn with
                                | None -> [0..cpu.clock - 1] 
                                            |> List.map (fun i -> { clock = i; xRegister = cpu.xRegister }) 
                                | Some prevCpu -> [prevCpu.clock + 1..cpu.clock - 1]
                                                    |> List.map (fun i -> { clock = i; xRegister = prevCpu.xRegister })

            let filled = [cpu] |> List.append cpusToAppend |> List.append filledIn

            AddMissingClockStates filled tail

let getClockScore (cpuIndex:int) (cpus:Cpu list) : int =
        let cpu = cpus[cpuIndex]
        
        if (cpuIndex = 0)
        then
            cpu.clock * cpu.xRegister.value
        else
            let prev = cpus[cpuIndex - 1]
            cpu.clock * prev.xRegister.value
        
let solve =
    // let lines = Common.getSampleDataAsArray 2022 10
    let lines = Common.getChallengeDataAsArray 2022 10

    let instructions = (lines |> Array.map lineToInstruction)

    let state = { cpuStack = [ cpu ]}

    let finalState = Array.fold processInstruction state instructions
    let cpus = List.rev finalState.cpuStack
    let filled = AddMissingClockStates [] cpus
    printfn "List has %d items" filled.Length

    let clockScoreMap = filled
                            |> List.mapi (fun i cpu -> 
                                            let score = getClockScore i filled
                                            (cpu.clock, score))
                            |> Map.ofList

    let cycles = [20; 60; 100; 140; 180; 220 ]
    let finalScore = cycles
                        |> List.fold (fun total cycle ->
                                        let subscore = Map.find cycle clockScoreMap
                                        total + subscore) 0
    printfn "Final score: %d" finalScore
        