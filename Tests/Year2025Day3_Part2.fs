module Tests.Year2025Day3_Part2

open System
open Xunit
open Year2025Day3_Part1


[<Theory>]
[<InlineData("123456789", 2, "89")>]
[<InlineData("1294867", 2, "98")>]
[<InlineData("1", 1, "1")>]
[<InlineData("987654321111111", 12, "987654321111")>]
[<InlineData("811111111111119", 12, "811111111119")>]
[<InlineData("234234234234278", 12, "434234234278")>]
[<InlineData("818181911112111", 12, "888911112111")>]
let ``can extract strings`` (input:string) (cellLength:int) (expected:string) : unit =
    let actual = getMaxVoltageSubstring input cellLength
    Assert.Equal(expected, actual)

