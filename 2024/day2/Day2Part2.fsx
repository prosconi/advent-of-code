#nowarn "00025"
// --- Part Two ---
// The engineers are surprised by the low number of safe reports until they realize they forgot to tell you about the Problem Dampener.

// The Problem Dampener is a reactor-mounted module that lets the reactor safety systems tolerate a single bad level in what would otherwise be a safe report. It's like the bad level never happened!

// Now, the same rules apply as before, except if removing a single level from an unsafe report would make it safe, the report instead counts as safe.

// More of the above example's reports are now safe:

// 7 6 4 2 1: Safe without removing any level.
// 1 2 7 8 9: Unsafe regardless of which level is removed.
// 9 7 6 2 1: Unsafe regardless of which level is removed.
// 1 3 2 4 5: Safe by removing the second level, 3.
// 8 6 4 4 1: Safe by removing the third level, 4.
// 1 3 6 7 9: Safe without removing any level.
// Thanks to the Problem Dampener, 4 reports are actually safe!

// Update your analysis by handling situations where the Problem Dampener can remove a single level from unsafe reports. How many reports are now safe?

open System
open System.IO

let exampleLines = 
    [|
        "7 6 4 2 1"
        "1 2 7 8 9"
        "9 7 6 2 1"
        "1 3 2 4 5"
        "8 6 4 4 1"
        "1 3 6 7 9"
    |]

let split (c: char) (s: string) = s.Split([| c |], StringSplitOptions.RemoveEmptyEntries)

let splitSpace = split ' '

let lines = 
    Path.Combine(__SOURCE_DIRECTORY__, "Day2.txt")
    |> File.ReadAllLines

let data = 
    lines
    |> Seq.map splitSpace
    |> Seq.map (Array.map int)
    |> Seq.toArray

let isSafe line = 
    let isIncreasing =
        line
        |> Array.windowed 2
        |> Array.forall (fun [|a; b|] -> 
            if a < b
            then b - a <= 3
            else false
        )

    let isDecreasing =
        line
        |> Array.windowed 2
        |> Array.forall (fun [|a; b|] -> 
            if a > b
            then a - b <= 3
            else false
        )

    isIncreasing || isDecreasing

let isSafer line =
    match isSafe line with
    | true -> true
    | false ->
        line
        |> Array.mapi (fun filterIndex _ ->
            [| line[0..filterIndex - 1]; line[filterIndex + 1..line.Length] |]
            |> Array.concat 
            |> isSafe
        )
        |> Array.tryFind id
        |> Option.isSome

data
|> Seq.filter isSafer
|> Seq.length
