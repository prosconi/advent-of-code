// --- Part Two ---
// The engineers seem concerned; the total calibration result you gave them is nowhere close to being within safety tolerances. Just then, you spot your mistake: some well-hidden elephants are holding a third type of operator.

// The concatenation operator (||) combines the digits from its left and right inputs into a single number. For example, 12 || 345 would become 12345. All operators are still evaluated left-to-right.

// Now, apart from the three equations that could be made true using only addition and multiplication, the above example has three more equations that can be made true by inserting operators:

// 156: 15 6 can be made true through a single concatenation: 15 || 6 = 156.
// 7290: 6 8 6 15 can be made true using 6 * 8 || 6 * 15.
// 192: 17 8 14 can be made true using 17 || 8 + 14.
// Adding up all six test values (the three that could be made before using only + and * plus the new three that can now be made by also using ||) produces the new total calibration result of 11387.

// Using your new knowledge of elephant hiding spots, determine which equations could possibly be true. What is their total calibration result?

open System
open System.IO

let exampleLines = 
    [|
        "190: 10 19"
        "3267: 81 40 27"
        "83: 17 5"
        "156: 15 6"
        "7290: 6 8 6 15"
        "161011: 16 10 13"
        "192: 17 8 14"
        "21037: 9 7 18 13"
        "292: 11 6 16 20"
    |]

let lines = 
    Path.Combine(__SOURCE_DIRECTORY__, "Day7.txt")
    |> File.ReadAllLines

let split (delimiter: string) (str: string) = 
    str.Split(delimiter, StringSplitOptions.RemoveEmptyEntries)

let parseLine (line: string) =
    match split ":" line with
    | [|a; b|] -> {| TestValue = int64 a; Equation = b |> split " " |> Array.map int64  |}
    | _ -> failwithf "Bad line: %A" line

let data =
    lines
    |> Array.map parseLine

let rec canSolve (equation: int64 []) (i: int) (sum: int64) (target: int64) =
    if i = equation.Length then
        sum = target
    else
        canSolve equation (i + 1) (sum + equation[i]) target
        || canSolve equation (i + 1) (sum * equation[i]) target
        || canSolve equation (i + 1) (int64 $"{sum}{equation[i]}") target

data
|> Array.sumBy (fun d ->
    match canSolve d.Equation 1 d.Equation[0] d.TestValue with
    | true -> d.TestValue
    | false -> 0L
)