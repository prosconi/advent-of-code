// --- Part Two ---
// The reindeer spends a few minutes reviewing your hiking trail map before realizing something, disappearing for a few minutes, and finally returning with yet another slightly-charred piece of paper.

// The paper describes a second way to measure a trailhead called its rating. A trailhead's rating is the number of distinct hiking trails which begin at that trailhead. For example:

// .....0.
// ..4321.
// ..5..2.
// ..6543.
// ..7..4.
// ..8765.
// ..9....
// The above map has a single trailhead; its rating is 3 because there are exactly three distinct hiking trails which begin at that position:

// .....0.   .....0.   .....0.
// ..4321.   .....1.   .....1.
// ..5....   .....2.   .....2.
// ..6....   ..6543.   .....3.
// ..7....   ..7....   .....4.
// ..8....   ..8....   ..8765.
// ..9....   ..9....   ..9....
// Here is a map containing a single trailhead with rating 13:

// ..90..9
// ...1.98
// ...2..7
// 6543456
// 765.987
// 876....
// 987....
// This map contains a single trailhead with rating 227 (because there are 121 distinct hiking trails that lead to the 9 on the right edge and 106 that lead to the 9 on the bottom edge):

// 012345
// 123456
// 234567
// 345678
// 4.6789
// 56789.
// Here's the larger example from before:

// 89010123
// 78121874
// 87430965
// 96549874
// 45678903
// 32019012
// 01329801
// 10456732
// Considering its trailheads in reading order, they have ratings of 20, 24, 10, 4, 1, 4, 5, 8, and 5. The sum of all trailhead ratings in this larger example topographic map is 81.

// You're not sure how, but the reindeer seems to have crafted some tiny flags out of toothpicks and bits of paper and is using them to mark trailheads on your topographic map. What is the sum of the ratings of all trailheads?

open System.IO

let example =
    [|
        "89010123"
        "78121874"
        "87430965"
        "96549874"
        "45678903"
        "32019012"
        "01329801"
        "10456732"
    |]

let example2 =
    [|
        "0123"
        "1034"
        "1115"
        "9876"
    |]

let lines = 
    Path.Combine(__SOURCE_DIRECTORY__, "Day10.txt")
    |> File.ReadAllLines

let data = 
    lines
    |> Seq.map (fun x -> x |> Seq.map string |> Seq.map int |> Seq.toArray)
    |> Seq.toArray

let findZeroes() =
    data
    |> Seq.indexed
    |> Seq.map (fun (y, row) ->
        row
        |> Seq.indexed
        |> Seq.filter(fun (_, col) -> col = 0)
        |> Seq.map fst
        |> Seq.map (fun x -> (x, y))
    )
    |> Seq.collect id

let up (x, y) = (x, y - 1)
let down (x, y) = (x, y + 1)
let left (x, y) = (x - 1, y)
let right (x, y) = (x + 1, y)
let width = data[0].Length
let height = data.Length
let inBounds (x, y) = 
    x >= 0 && x < width
        && y >= 0 && y < height

let getValue (x, y) =
    if inBounds (x, y)
    then Some(data[y][x])
    else None

let rec go position path (count: int) =
    [|
        for action in [left; right; up; down] do
            let newPosition = action position
            match getValue newPosition with
            | Some c ->
                if c = count + 1 then
                    if c = 9
                    then yield ((c, newPosition) :: path)
                    else yield! go newPosition ((c, newPosition) :: path) (count + 1)
                else 
                    ()
            | None -> 
                ()
    |]
    |> Array.distinct
    

findZeroes()
|> Seq.map (fun (x, y) -> go (x, y) [] 0)
|> Seq.toArray
|> Array.map (fun x -> x.Length)
|> Seq.sum
