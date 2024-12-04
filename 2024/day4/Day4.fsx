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

let getChar (x, y) = 
    match inBounds'(x, y) with
    | true -> data[y][x]
    | false -> ' '

let count nextFn c0 =
    let c1 = nextFn c0
    let c2 = nextFn c1
    let c3 = nextFn c2
    let word = $"{getChar c0}{getChar c1}{getChar c2}{getChar c3}"
    match word with
    | "XMAS" -> 1
    | _ -> 0

let countXmas coords =
    count left coords +
        count right coords +
        count up coords + 
        count down coords +
        count upLeft coords +
        count upRight coords +
        count downLeft coords +
        count downRight coords

seq {
    for y = 0 to data.Length - 1 do
        for x = 0 to data[y].Length - 1 do
            countXmas(x, y)
}
|> Seq.sum
