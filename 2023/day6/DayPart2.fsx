// 2--- Part Two ---
// As the race is about to start, you realize the piece of paper with race times and record distances you got earlier actually just has very bad kerning. There's really only one race - ignore the spaces between the numbers on each line.

// So, the example from before:

// Time:      7  15   30
// Distance:  9  40  200
// ...now instead means this:

// Time:      71530
// Distance:  940200
// Now, you have to figure out how many ways there are to win this single race. In this example, the race lasts for 71530 milliseconds and the record distance you need to beat is 940200 millimeters. You could hold the button anywhere from 14 to 71516 milliseconds and beat the record, a total of 71503 ways!

// How many ways can you beat the record in this one much longer race?

open System
open System.IO

let example() =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Example6.txt"))

let readInputFile() =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Day6.txt"))

let splitIntoTwo (delimiter: string) (line: string) =
    let index = line.IndexOf(delimiter)
    let left = line.Substring(0, index)
    let right = line.Substring(index + delimiter.Length)
    left, right

let splitAndParse (str: string) =
    str.Replace(" ", "")
    |> int64

let getWinners (totalTime: int64, distance: int64) =
    [|
        for timeCharging = 0L to totalTime do
            let speed = totalTime - (totalTime - timeCharging)
            let timeMoving = totalTime - timeCharging
            let distanceTraveled = speed * timeMoving
            if distanceTraveled > distance then
                yield {| 
                        timeCharging = timeCharging
                        speed = speed
                        timeMoving = timeMoving
                        distanceTraveled = distanceTraveled
                      |}
    |]

let parseLines (lines: string[]) =
    let _, times = lines |> Array.find (fun x -> x.StartsWith("Time:")) |> splitIntoTwo ":"
    let _, distances = lines |> Array.find (fun x -> x.StartsWith("Distance:")) |> splitIntoTwo ":"
    let time = times |> splitAndParse
    let distance = distances |> splitAndParse
    [| time, distance |]

let go() =
    readInputFile()
    |> Seq.toArray
    |> parseLines
    |> Array.map getWinners
    |> Array.map (fun x -> x.Length)
    |> Array.map int64
    |> Array.reduce (*)

go()
