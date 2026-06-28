module Tests.Year2025Day1_Part2

open System
open Xunit
open Year2025Day1_Part1


[<Fact>]
let ``can turn left and land on zero`` () =
    let state:State =
        {
            currentNumber = 55; moveCount = 0; zeroCount = 0; zeroClicks = 0
        }

    let rotation = (-1, 55)
    let nextState = processPart2Rotation state rotation

    Assert.Equal(1, nextState.zeroClicks)

[<Fact>]
let ``can turn left one full rotation`` () =
    let state:State =
        {
            currentNumber = 50; moveCount = 0; zeroCount = 0; zeroClicks = 0
        }

    let rotation = (-1, DIAL_SIZE)
    let nextState = processPart2Rotation state rotation

    Assert.Equal(1, nextState.zeroClicks)

[<Fact>]
let ``can turn left one full rotation at zero`` () =
    let state:State =
        {
            currentNumber = 0; moveCount = 0; zeroCount = 0; zeroClicks = 0
        }

    let rotation = (-1, DIAL_SIZE)
    let nextState = processPart2Rotation state rotation

    Assert.Equal(1, nextState.zeroClicks)

[<Fact>]
let ``does not double count when going left from 0`` () =
    let state:State =
        {
            currentNumber = 0; moveCount = 0; zeroCount = 0; zeroClicks = 0
        }

    let rotation = (-1, 1)
    let nextState = processPart2Rotation state rotation

    Assert.Equal(99, nextState.currentNumber)
    Assert.Equal(0, nextState.zeroClicks)

[<Fact>]
let ``can turn right one full rotation at zero`` () =
    let state:State =
        {
            currentNumber = 0; moveCount = 0; zeroCount = 0; zeroClicks = 0
        }

    let rotation = (1, DIAL_SIZE)
    let nextState = processPart2Rotation state rotation

    Assert.Equal(1, nextState.zeroClicks)

[<Fact>]
let ``can do 50 r 1000`` () =
    let state:State =
        {
            currentNumber = 50; moveCount = 0; zeroCount = 0; zeroClicks = 0
        }

    let rotation = (1, 1000)
    let nextState = processPart2Rotation state rotation

    Assert.Equal(10, nextState.zeroClicks)