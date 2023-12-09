// --- Part Two ---
// Of course, it would be nice to have even more history included in your report. Surely it's safe to just extrapolate backwards as well, right?

// For each history, repeat the process of finding differences until the sequence of differences is entirely zero. Then, rather than adding a zero to the end and filling in the next values of each previous sequence, you should instead add a zero to the beginning of your sequence of zeroes, then fill in new first values for each previous sequence.

// In particular, here is what the third example history looks like when extrapolating back in time:

// 5  10  13  16  21  30  45
//   5   3   3   5   9  15
//    -2   0   2   4   6
//       2   2   2   2
//         0   0   0
// Adding the new values on the left side of each sequence from bottom to top eventually reveals the new left-most history value: 5.

// Doing this for the remaining example data above results in previous values of -3 for the first history and 0 for the second history. Adding all three new values together produces 2.

// Analyze your OASIS report again, this time extrapolating the previous value for each history. What is the sum of these extrapolated values?

open System.IO

let example() =
    [|
        "0 3 6 9 12 15"
        "1 3 6 10 15 21"
        "10 13 16 21 30 45"
    |]

let readInputFile() =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Day9.txt"))

let parse (lines: string seq) =
    lines
    |> Seq.map (fun line -> line.Split ' ' |> Array.map int)

let processLine originalLine =
    [|
        let mutable completed = false
        let mutable line = originalLine
        yield originalLine
        while not completed do
            let solution = line |> Array.windowed 2 |> Array.map (fun a -> a.[1] - a.[0])
            completed <- solution |> Array.forall ((=) 0)
            line <- solution
            yield line
    |]

let solve (lines: int[][]) =
    let mutable i = lines.Length - 1
    let mutable last = 0
    [|
        yield [| yield! lines[i]; 0 |]
        while i > 0 do
            let previousLine = lines[i - 1]
            last <- (previousLine |> Array.head) - last
            i <- i - 1
            yield [| last; yield! previousLine |]
    |]
    |> Array.rev

readInputFile()
|> parse
|> Seq.toArray
|> Array.map processLine
|> Array.map solve
|> Array.map (fun x -> x |> Array.head |> Array.head)
|> Array.map int64
|> Array.sum