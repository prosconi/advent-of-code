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

open System.IO

let digitWords = 
    [|
        "1", 1
        "2", 2
        "3", 3
        "4", 4
        "5", 5
        "6", 6
        "7", 7
        "8", 8
        "9", 9
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

let day2Example =
    [|
        "two1nine", 29
        "eightwothree", 83
        "abcone2threexyz", 13
        "xtwone3four", 24
        "4nineeightseven2", 42
        "zoneight234", 14
        "7pqrstsixteen", 76
    |]

let testCases =
    [|
        "oneight", 18
        "fourtwo15nine1", 41
    |]

let readDay1Input() = 
    File.ReadAllLines("Day1.txt")

let log x = printfn "%A" x; x
type Position =
    {
        Index: int
        Word: string
        Digit: int
    }

type Pipeline =
    {
        Input: string
        Positions: Position[]
        MinPosition: Position
        MaxPosition: Position
        Digits: string
    }

let part2Answer = 
    readDay1Input()
    |> Seq.map (fun x ->
        let positions = 
            digitWords
            |> Array.collect (fun word -> 
                [|
                    x.IndexOf(fst word), word
                    x.LastIndexOf(fst word), word
                |]
            )
            |> Array.filter (fun (index, _) -> index >= 0)
            |> Array.map (fun (index, (word, digit)) -> { Index = index; Word = word; Digit = digit })

        let minPosition = positions |> Array.minBy (fun x -> x.Index)
        let maxPosition = positions |> Array.maxBy (fun x -> x.Index)
        let min = minPosition.Digit
        let max = maxPosition.Digit
        let digits = $"{min}{max}"

        { Input = x
          Positions = positions
          MinPosition = minPosition
          MaxPosition = maxPosition
          Digits = digits }
    )
    |> Seq.map log
    |> Seq.sumBy (fun x -> int x.Digits)

// 53846 -- wrong
// 53921 -- wrong
// 53855