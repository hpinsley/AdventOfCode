module Tests.Year2025Day6_Part2

open System
open Xunit
open Year2025Day6_Part1


// [<Theory>]
// [<InlineData("123456789", 2, "89")>]
// [<InlineData("1294867", 2, "98")>]
// [<InlineData("1", 1, "1")>]
// [<InlineData("987654321111111", 12, "987654321111")>]
// [<InlineData("811111111111119", 12, "811111111119")>]
// [<InlineData("234234234234278", 12, "434234234278")>]
// [<InlineData("818181911112111", 12, "888911112111")>]
// let ``can extract strings`` (input:string) (cellLength:int) (expected:string) : unit =
//     let actual = getMaxVoltageSubstring cellLength input
//     Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("64,23,314", "4,431,623")>]
[<InlineData("51,387,215", "175,581,32")>]
let ``can convert part1 operands to part 2 operands`` (input: string) (expectedOutput:string): unit =
    let intputOperands = input.Split(",")
                                            |> Array.map Operand.Parse
    let expectedOutputOperands = expectedOutput.Split(",")
                                    |> Array.map Operand.Parse

    let outputOperands = convertPart1OperandsToPart2 intputOperands
                                    
    Assert.Equal (expectedOutputOperands.Length, outputOperands.Length)
    expectedOutputOperands
        |> Array.iteri (fun index expected -> Assert.Equal(expected, outputOperands[index]))

