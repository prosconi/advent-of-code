// --- Part Two ---
// The Elf looks quizzically at you. Did you misunderstand the assignment?

// Looking for the instructions, you flip over the word search to find that this isn't actually an XMAS puzzle; it's an X-MAS puzzle in which you're supposed to find two MAS in the shape of an X. One way to achieve that is like this:

// M.S
// .A.
// M.S
// Irrelevant characters have again been replaced with . in the above diagram. Within the X, each MAS can be written forwards or backwards.

// Here's the same example from before, but this time all of the X-MASes have been kept instead:

// .M.S......
// ..A..MSMS.
// .M.S.MAA..
// ..A.ASMSM.
// .M.S.M....
// ..........
// S.S.S.S.S.
// .A.A.A.A..
// M.M.M.M.M.
// ..........
// In this example, an X-MAS appears 9 times.

// Flip the word search from the instructions back over to the word search side and try again. How many times does an X-MAS appear?

open System.IO

let exampleLines = 
    [|
        "MMMSXXMASM"
        "MSAMXMSMSA"
        "AMXSXMAAMM"
        "MSAMASMSMX"
        "XMASAMXAMM"
        "XXAMMXXAMA"
        "SMSMSASXSS"
        "SAXAMASAAA"
        "MAMMMXMMMM"
        "MXMXAXMASX"
    |]

let upLeft (x, y) = (x - 1, y - 1)
let upRight (x, y) = (x + 1, y - 1)
let downLeft (x, y) = (x - 1, y + 1)
let downRight (x, y) = (x + 1, y + 1)
let inBounds (width, height) (x, y) = x >= 0 && x < width && y >= 0 && y < height

let lines = 
    Path.Combine(__SOURCE_DIRECTORY__, "Day4.txt")
    |> File.ReadAllLines

let data = lines

let inBounds' = inBounds (data[0].Length, data.Length)

let getChar (x, y) = 
    match inBounds'(x, y) with
    | true -> data[y][x]
    | false -> ' '

let count currentCoord =
    match getChar currentCoord with
    | 'A' ->
        let ul = getChar(upLeft currentCoord)
        let dr = getChar(downRight currentCoord)
        let ur = getChar(upRight currentCoord)
        let dl = getChar(downLeft currentCoord)
        match ul, dr with
        | 'M', 'S'
        | 'S', 'M' ->
            match ur, dl with
            | 'M', 'S'
            | 'S', 'M' -> 1
            | _ -> 0
        | _ -> 0
    | _ -> 0

seq {
    for y = 0 to data.Length - 1 do
        for x = 0 to data[y].Length - 1 do
            count(x, y)
}
|> Seq.sum
