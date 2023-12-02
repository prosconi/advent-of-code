// --- Part Two ---
// Your calculation isn't quite right. It looks like some of the digits are actually spelled out with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".

// Equipped with this new information, you now need to find the real first and last digit on each line. For example:

// two1nine
// eightwothree
// abcone2threexyz
// xtwone3four
// 4nineeightseven2
// zoneight234
// 7pqrstsixteen
// In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.

// What is the sum of all of the calibration values?

open System
open System.IO

let digitWords = 
    [|
        "one", 1
        "two", 2
        "three", 3
        "four", 4
        "five", 5
        "six", 6
        "seven", 7
        "eight", 8
        "nine", 9
    |]

let normalizeString (s: string) =
    let rec normalizeString' (s: string) =
        let replacement = 
            digitWords
            |> Array.map (fun word -> s.IndexOf(fst word), word)
            |> Array.filter (fun (index, _) -> index >= 0)

        match replacement with
        | [||] -> s |> String.filter Char.IsDigit
        | other ->
            let index, (word, replacement) = other |> Array.minBy fst
            let left = s.Substring(0, index)
            let middle = string replacement
            let right = s.Substring(index + word.Length)
            normalizeString' ($"{left}{middle}{right}")
    
    normalizeString' (s.ToLower())

let parseDigits line =
    let digits = line |> String.filter Char.IsDigit |> Seq.toArray
    let first, last = (digits |> Seq.head), (digits |> Seq.last)
    int $"{first}{last}"

let day2Example =
    [|
        "two1nine"
        "eightwothree"
        "abcone2threexyz"
        "xtwone3four"
        "4nineeightseven2"
        "zoneight234"
        "7pqrstsixteen"
    |]

let readDay1Input() = 
    File.ReadAllLines("Day1.txt")

let day2Answer = 
    readDay1Input()
    |> Seq.map (fun x -> x, normalizeString x)
    |> Seq.map (fun (original, x) -> original, x, parseDigits x)
    |> Seq.sumBy (fun (_, _, x) -> x)

// 53846 -- wrong
