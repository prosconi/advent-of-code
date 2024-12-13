#nowarn "00025"
// --- Day 13: Claw Contraption ---
// Next up: the lobby of a resort on a tropical island. The Historians take a moment to admire the hexagonal floor tiles before spreading out.

// Fortunately, it looks like the resort has a new arcade! Maybe you can win some prizes from the claw machines?

// The claw machines here are a little unusual. Instead of a joystick or directional buttons to control the claw, these machines have two buttons labeled A and B. Worse, you can't just put in a token and play; it costs 3 tokens to push the A button and 1 token to push the B button.

// With a little experimentation, you figure out that each machine's buttons are configured to move the claw a specific amount to the right (along the X axis) and a specific amount forward (along the Y axis) each time that button is pressed.

// Each machine contains one prize; to win the prize, the claw must be positioned exactly above the prize on both the X and Y axes.

// You wonder: what is the smallest number of tokens you would have to spend to win as many prizes as possible? You assemble a list of every machine's button behavior and prize location (your puzzle input). For example:

// Button A: X+94, Y+34
// Button B: X+22, Y+67
// Prize: X=8400, Y=5400

// Button A: X+26, Y+66
// Button B: X+67, Y+21
// Prize: X=12748, Y=12176

// Button A: X+17, Y+86
// Button B: X+84, Y+37
// Prize: X=7870, Y=6450

// Button A: X+69, Y+23
// Button B: X+27, Y+71
// Prize: X=18641, Y=10279
// This list describes the button configuration and prize location of four different claw machines.

// For now, consider just the first claw machine in the list:

// Pushing the machine's A button would move the claw 94 units along the X axis and 34 units along the Y axis.
// Pushing the B button would move the claw 22 units along the X axis and 67 units along the Y axis.
// The prize is located at X=8400, Y=5400; this means that from the claw's initial position, it would need to move exactly 8400 units along the X axis and exactly 5400 units along the Y axis to be perfectly aligned with the prize in this machine.
// The cheapest way to win the prize is by pushing the A button 80 times and the B button 40 times. This would line up the claw along the X axis (because 80*94 + 40*22 = 8400) and along the Y axis (because 80*34 + 40*67 = 5400). Doing this would cost 80*3 tokens for the A presses and 40*1 for the B presses, a total of 280 tokens.

// For the second and fourth claw machines, there is no combination of A and B presses that will ever win a prize.

// For the third claw machine, the cheapest way to win the prize is by pushing the A button 38 times and the B button 86 times. Doing this would cost a total of 200 tokens.

// So, the most prizes you could possibly win is two; the minimum tokens you would have to spend to win all (two) prizes is 480.

// You estimate that each button would need to be pressed no more than 100 times to win a prize. How else would someone be expected to play?

// Figure out how to win as many prizes as possible. What is the fewest tokens you would have to spend to win all possible prizes?

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
    |> int

let parsePrizeXY (str: string) =
    str
    |> splitIntoTwo '=' 
    |> snd
    |> int

let parseButton (str: string) =
    let x, y = splitIntoTwo ',' str
    parseXY x, parseXY y

let parsePrize (str: string) =
    let x, y = splitIntoTwo ',' str
    parsePrizeXY x, parsePrizeXY y

type Solution =
    {
        ButtonA: (int * int)
        ButtonB: (int * int)
        Prize: (int * int)
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

let cost (aCount, bCount) = aCount * 3 + bCount

let solve prize buttonOne buttonTwo =
    let mutable x, y = 0, 0
    let mutable dx1, dy1 = buttonOne
    let mutable dx2, dy2 = buttonTwo
    let mutable presses = 0
    let mutable exit = false
    let mutable otherPressesX = 0
    let mutable otherPressesY = 0
    let mutable prizeX, prizeY = prize
    while (x < prizeX && y < prizeY) && not exit do
        if (prizeX - x) % dx2 = 0 && (prizeY - y) % dy2 = 0 then
            otherPressesX <- (prizeX - x) / dx2
            otherPressesY <- (prizeY - y) / dy2
            if otherPressesX <> otherPressesY then 
                x <- x + dx1
                y <- y + dy1
                presses <- presses + 1
            else
                exit <- true
        else
            x <- x + dx1
            y <- y + dy1
            presses <- presses + 1

    if exit
    then Some (presses, otherPressesX)
    else None

data
|> Seq.choose (fun { ButtonA = a; ButtonB = b; Prize = p } -> solve p a b)
|> Seq.map cost
|> Seq.sum
