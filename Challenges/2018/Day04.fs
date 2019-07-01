module Year2018Day04

open System.IO
open System.Text.RegularExpressions

let getData (file: string): array<string> =
    printf "Reading from file %s\n" file
    File.ReadAllLines file

let rec parseGuardData guard startSleep events =
    let beginShiftPattern = "\[(\d\d\d\d)-(\d\d)-(\d\d) (\d\d):(\d\d)\] Guard #(\d+) begins shift"
    let beginSleepPattern = "\[(\d\d\d\d)-(\d\d)-(\d\d) (\d\d):(\d\d)\] falls asleep"
    let beginAwakePattern = "\[(\d\d\d\d)-(\d\d)-(\d\d) (\d\d):(\d\d)\] wakes up"

    seq {
        match events with
            | [] ->
                ()

            | event :: remaining ->
                let mGuard = Regex.Match(event, beginShiftPattern)
                if mGuard.Success && mGuard.Groups.Count = 7 then
                    let newGuard = int mGuard.Groups.[6].Value
                    yield! parseGuardData newGuard startSleep remaining

                let mSleep = Regex.Match(event, beginSleepPattern)
                if mSleep.Success && mSleep.Groups.Count = 6 then
                    let minute = int mSleep.Groups.[5].Value
                    yield! parseGuardData guard minute remaining

                let mWake = Regex.Match(event, beginAwakePattern)
                if mWake.Success && mWake.Groups.Count = 6 then
                    let awakeAt = int mWake.Groups.[5].Value
                    yield (guard, startSleep, awakeAt, awakeAt - startSleep)
                    yield! parseGuardData guard startSleep remaining

    }

let expandToMinutes (guard, startSleep, endSleep, duration) =
    seq {
        for s in startSleep..(endSleep-1) do
        yield (guard, s)
    }

let getSleepiestMinuteAndTimes (sleepTimes: (int * int) list) =
    sleepTimes
        |> List.countBy (fun (guard, minute) -> minute)
        |> List.maxBy (fun (minute, count) -> count)

let solve =
    let textFile = "InputFiles/input-2018-04.txt"
    let values = getData textFile
                  |> Array.toList
                  |> List.sort

    for line in values do
        printfn "%s" line

    printfn "Read %d lines" (List.length values)

    let guardNaps = parseGuardData 0 0 values
                    |> Seq.toList

    let sleepTimeByGuard =
        guardNaps
            |> List.groupBy (fun (guard, _, _, _) -> guard)
            |> List.map (fun (guardNo, guardNaps) ->
                            let slept = List.sumBy (fun (g, s, e, dur) -> dur) guardNaps
                            (guardNo, slept)
                )

            |> List.sortBy(fun (guard, slept) -> slept)

    printfn "Sleep time by guard"
    for line in sleepTimeByGuard do
        printfn "%A" line

    let sleepiest = List.last sleepTimeByGuard |> fst

    printfn "The sleepiest guard is %d" sleepiest

    let naps =
        guardNaps
            |> List.filter (fun (guard, s, e, dur) -> guard = sleepiest)
            |> Seq.collect expandToMinutes
            |> Seq.toList

    printfn "Expanded to %d naps" (List.length naps)

    let (sleepyMinute, times) =
        naps
            |> List.countBy (fun (guard, minute) -> minute)
            |> List.maxBy (fun (minute, count) -> count)

    printfn "The sleepiest guard (%d) sleep %d times during minute %d" sleepiest sleepyMinute times
    let result = sleepiest * sleepyMinute

    printfn "So the answer to part one is %d x %d = %d" sleepiest sleepyMinute result

    let allGuardNaps =
        guardNaps
            |> Seq.collect expandToMinutes
            |> Seq.toList
            |> List.groupBy (fun (guard, minute) -> guard)
            |> List.map (fun (g, naps) -> (g, getSleepiestMinuteAndTimes naps))

    printfn "Here are all the guard's sleepiest times"

    for s in allGuardNaps do
        printfn "%A" s

    let (guard, (minute, times)) =
            allGuardNaps
                |> List.maxBy (fun (guard, (minute, times)) -> times)

    printfn "Best is guard %d at minute %d" guard minute
    printfn "Part 2 solution is %d" (guard * minute)
