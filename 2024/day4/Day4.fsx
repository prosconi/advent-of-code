// --- Day 4: Ceres Search ---
// "Looks like the Chief's not here. Next!" One of The Historians pulls out a device and pushes the only button on it. After a brief flash, you recognize the interior of the Ceres monitoring station!

// As the search for the Chief continues, a small Elf who lives on the station tugs on your shirt; she'd like to know if you could help her with her word search (your puzzle input). She only has to find one word: XMAS.

// This word search allows words to be horizontal, vertical, diagonal, written backwards, or even overlapping other words. It's a little unusual, though, as you don't merely need to find one instance of XMAS - you need to find all of them. Here are a few ways XMAS might appear, where irrelevant characters have been replaced with .:


// ..X...
// .SAMX.
// .A..A.
// XMAS.S
// .X....
// The actual word search will be full of letters instead. For example:

// MMMSXXMASM
// MSAMXMSMSA
// AMXSXMAAMM
// MSAMASMSMX
// XMASAMXAMM
// XXAMMXXAMA
// SMSMSASXSS
// SAXAMASAAA
// MAMMMXMMMM
// MXMXAXMASX
// In this word search, XMAS occurs a total of 18 times; here's the same word search again, but where letters not involved in any XMAS have been replaced with .:

// ....XXMAS.
// .SAMXMS...
// ...S..A...
// ..A.A.MS.X
// XMASAMX.MM
// X.....XA.A
// S.S.S.S.SS
// .A.A.A.A.A
// ..M.M.M.MM
// .X.X.XMASX
// Take a look at the little Elf's word search. How many times does XMAS appear?

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

let up (x, y) = (x, y - 1)
let down (x, y) = (x, y + 1)
let left (x, y) = (x - 1, y)
let right (x, y) = (x + 1, y)
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

let word = "XMAS"

let rec count nextFn letterIndex currentCoord =
    let nextCoord = nextFn currentCoord
    let isInBounds = inBounds' currentCoord
    let currentChar = word[letterIndex]

    match isInBounds with
    | true ->
        let x, y = currentCoord
        let c = data[y][x]
        match c = currentChar with 
        | true ->
            match letterIndex = word.Length - 1 with
            | true -> 1
            | false -> count nextFn (letterIndex + 1) nextCoord
        | false -> 0
    | false ->
        0

let countXmas coords =
    count left 0 coords +
        count right 0 coords +
        count up 0 coords + 
        count down 0 coords +
        count upLeft 0 coords +
        count upRight 0 coords +
        count downLeft 0 coords +
        count downRight 0 coords

seq {
    for y = 0 to data.Length - 1 do
        for x = 0 to data[y].Length - 1 do
            countXmas(x, y)
}
|> Seq.sum
