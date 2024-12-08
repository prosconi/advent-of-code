// --- Part Two ---
// Watching over your shoulder as you work, one of The Historians asks if you took the effects of resonant harmonics into your calculations.

// Whoops!

// After updating your model, it turns out that an antinode occurs at any grid position exactly in line with at least two antennas of the same frequency, regardless of distance. This means that some of the new antinodes will occur at the position of each antenna (unless that antenna is the only one of its frequency).

// So, these three T-frequency antennas now create many antinodes:

// T....#....
// ...T......
// .T....#...
// .........#
// ..#.......
// ..........
// ...#......
// ..........
// ....#.....
// ..........
// In fact, the three T-frequency antennas are all exactly in line with two antennas, so they are all also antinodes! This brings the total number of antinodes in the above example to 9.

// The original example now has 34 antinodes, including the antinodes that appear on every antenna:

// ##....#....#
// .#.#....0...
// ..#.#0....#.
// ..##...0....
// ....0....#..
// .#...#A....#
// ...#..#.....
// #....#.#....
// ..#.....A...
// ....#....A..
// .#........#.
// ...#......##
// Calculate the impact of the signal using this updated model. How many unique locations within the bounds of the map contain an antinode?

open System.Collections.Generic
open System.IO

let exampleLines = 
    [|
        "............"
        "........0..."
        ".....0......"
        ".......0...."
        "....0......."
        "......A....."
        "............"
        "............"
        "........A..."
        ".........A.."
        "............"
        "............"
    |]

let lines = 
    Path.Combine(__SOURCE_DIRECTORY__, "Day8.txt")
    |> File.ReadAllLines

let data = lines

let width = data[0].Length
let height = data.Length

let inBounds (x, y) = x >= 0 && x < width && y >= 0 && y < height

data
|> Array.mapi (fun y v ->
    v
    |> Seq.mapi (fun x v -> 
        if v = '.'
        then None
        else Some({| Char = v; Point = x, y |})
    )
    |> Seq.choose id
)
|> Seq.collect id
|> Seq.groupBy _.Char
|> Seq.map (fun (k, v) -> 
    let positions = Dictionary<_,_>()
    for x1 in v do
        for x2 in v do
            if x1 <> x2 then
                let px1, py1 = x1.Point
                let px2, py2 = x2.Point
                let dx = px2 - px1
                let dy = py2 - py1
                let mutable newPoint = px1 + dx, py1 + dy
                while inBounds newPoint do
                    positions[newPoint] <- true
                    let newX, newY = newPoint
                    newPoint <- newX + dx, newY + dy

    positions.Keys
)
|> Seq.collect id
|> Seq.distinct
|> Seq.filter inBounds
|> Seq.toArray
|> Array.length