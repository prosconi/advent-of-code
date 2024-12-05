#nowarn "00025"
// --- Part Two ---
// While the Elves get to work printing the correctly-ordered updates, you have a little time to fix the rest of them.

// For each of the incorrectly-ordered updates, use the page ordering rules to put the page numbers in the right order. For the above example, here are the three incorrectly-ordered updates and their correct orderings:

// 75,97,47,61,53 becomes 97,75,47,61,53.
// 61,13,29 becomes 61,29,13.
// 97,13,75,29,47 becomes 97,75,47,29,13.
// After taking only the incorrectly-ordered updates and ordering them correctly, their middle page numbers are 47, 29, and 47. Adding these together produces 123.

// Find the updates which are not in the correct order. What do you get if you add up the middle page numbers after correctly ordering just those updates?

open System.IO
open System

let exampleLines = 
    [|
        "47|53"
        "97|13"
        "97|61"
        "97|47"
        "75|29"
        "61|13"
        "75|53"
        "29|13"
        "97|29"
        "53|29"
        "61|53"
        "97|53"
        "61|29"
        "47|13"
        "75|47"
        "97|75"
        "47|61"
        "75|61"
        "47|29"
        "75|13"
        "53|13"
        ""
        "75,47,61,53,29"
        "97,61,53,29,13"
        "75,29,13"
        "75,97,47,61,53"
        "61,13,29"
        "97,13,75,29,47"
    |]

type Line =
    | Rule of int * int
    | Update of int[]
    | Empty

let parseLine (str: string) =
    match str with
    | "" -> Empty
    | _ ->
        if str.Contains "|" then
            match str.Split '|' with
            | [| a; b |] -> Rule(int a, int b)
            | _ -> failwith "Unexpected split"
        else Update(str.Split ',' |> Array.map int)

let lines = 
    Path.Combine(__SOURCE_DIRECTORY__, "Day5.txt")
    |> File.ReadAllLines

let data = lines

let parsed = data |> Array.map parseLine

let rules = parsed |> Array.choose (function | Rule(a, b) -> Some(a, b) | _ -> None)

let updates = parsed |> Array.choose (function | Update u -> Some u | _ -> None)

let rulesDict =
    rules
    |> Array.groupBy fst
    |> Array.map (fun (k, v) -> (k, v |> Array.map snd))
    |> dict

let isValid (update: int[]) =
    update
    |> Array.windowed 2
    |> Array.forall (fun [|a; b|] ->
        match rulesDict.TryGetValue a with
        | true, values -> Array.contains b values
        | _ -> false
    )

let getMid (update: int[]) =
    update[update.Length / 2]

let pageCompare a b =
    if a = b
    then 0
    else
        match rulesDict.TryGetValue(a) with
        | true, values -> 
            if Array.contains b values
            then 1
            else -1
        | _ -> -1

updates
|> Seq.filter (not << isValid)
|> Seq.map (Array.sortWith pageCompare)
|> Seq.map getMid
|> Seq.sum
