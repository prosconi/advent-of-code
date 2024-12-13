#nowarn "00025"
// --- Part Two ---
// As you go to win the first prize, you discover that the claw is nowhere near where you expected it would be. Due to a unit conversion error in your measurements, the position of every prize is actually 10000000000000 higher on both the X and Y axis!

// Add 10000000000000 to the X and Y position of every prize. After making this change, the example above would now look like this:

// Button A: X+94, Y+34
// Button B: X+22, Y+67
// Prize: X=10000000008400, Y=10000000005400

// Button A: X+26, Y+66
// Button B: X+67, Y+21
// Prize: X=10000000012748, Y=10000000012176

// Button A: X+17, Y+86
// Button B: X+84, Y+37
// Prize: X=10000000007870, Y=10000000006450

// Button A: X+69, Y+23
// Button B: X+27, Y+71
// Prize: X=10000000018641, Y=10000000010279
// Now, it is only possible to win a prize on the second and fourth claw machines. Unfortunately, it will take many more than 100 presses to do so.

// Using the corrected prize coordinates, figure out how to win as many prizes as possible. What is the fewest tokens you would have to spend to win all possible prizes?

open System.IO

let example = 
    [|
        "Button A: X+94, Y+34"
        "Button B: X+22, Y+67"
        "Prize: X=8400, Y=5400"
        ""
        "Button A: X+26, Y+66"
        "Button B: X+67, Y+21"
        "Prize: X=12748, Y=12176"
        ""
        "Button A: X+17, Y+86"
        "Button B: X+84, Y+37"
        "Prize: X=7870, Y=6450"
        ""
        "Button A: X+69, Y+23"
        "Button B: X+27, Y+71"
        "Prize: X=18641, Y=10279"
        ""
    |]

let splitIntoTwo (ch: char) (str: string) = 
    match str.Split ch with
    | [| partA; partB |] -> (partA, partB)
    | _ -> failwithf "Error in code: %s, ch=%A" str ch

let parseXY (str: string) =
    str
    |> splitIntoTwo '+' 
    |> snd
    |> int64

let parsePrizeXY (str: string) =
    str
    |> splitIntoTwo '=' 
    |> snd
    |> int64

let parseButton (str: string) =
    let x, y = splitIntoTwo ',' str
    parseXY x, parseXY y

let parsePrize (str: string) =
    let x, y = splitIntoTwo ',' str
    parsePrizeXY x + 10000000000000L, parsePrizeXY y + 10000000000000L

type Claw =
    {
        ButtonA: (int64 * int64)
        ButtonB: (int64 * int64)
        Prize: (int64 * int64)
    }

let fileLines = 
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Day13.txt"))

let data =
    fileLines
    |> Seq.filter (fun x -> x <> "")
    |> Seq.chunkBySize 3
    |> Seq.map (fun [| buttonAString; buttonBString; prizeString |] -> 
        let buttonA = buttonAString |> splitIntoTwo ':' |> snd |> parseButton
        let buttonB = buttonBString |> splitIntoTwo ':' |> snd |> parseButton
        let prize = prizeString |> splitIntoTwo ':' |> snd |> parsePrize
        { ButtonA = buttonA; ButtonB = buttonB; Prize = prize }
    )
    |> Seq.toArray

let solve (claw: Claw) =
    let aX = decimal <| fst claw.ButtonA
    let aY = decimal <| snd claw.ButtonA
    let bX = decimal <| fst claw.ButtonB
    let bY = decimal <| snd claw.ButtonB
    let prizeX = decimal <| fst claw.Prize
    let prizeY = decimal <| snd claw.Prize

    let det = aX * bY - aY * bX
    let a = (bY * prizeX - bX * prizeY) / det
    let b = (aX * prizeY - aY * prizeX) / det
    if a % 1M = 0M && b % 1M = 0M then
        3L * int64 a + int64 b
    else
        0L

data
|> Seq.map solve
|> Seq.sum
