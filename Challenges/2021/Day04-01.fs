﻿module Year2021Day4_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

let solve =
    let lines = Common.getSampleDataAsArray 2021 4
    // let lines = Common.getChallengeDataAsArray 2021 4
    printAllLines lines
    ()