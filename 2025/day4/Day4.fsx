// --- Day 4: Printing Department ---
// You ride the escalator down to the printing department. They're clearly getting ready for Christmas; they have lots of large rolls of paper everywhere, and there's even a massive printer in the corner (to handle the really big print jobs).

// Decorating here will be easy: they can make their own decorations. What you really need is a way to get further into the North Pole base while the elevators are offline.

// "Actually, maybe we can help with that," one of the Elves replies when you ask for help. "We're pretty sure there's a cafeteria on the other side of the back wall. If we could break through the wall, you'd be able to keep moving. It's too bad all of our forklifts are so busy moving those big rolls of paper around."

// If you can optimize the work the forklifts are doing, maybe they would have time to spare to break through the wall.

// The rolls of paper (@) are arranged on a large grid; the Elves even have a helpful diagram (your puzzle input) indicating where everything is located.

// For example:

// ..@@.@@@@.
// @@@.@.@.@@
// @@@@@.@.@@
// @.@@@@..@.
// @@.@@@@.@@
// .@@@@@@@.@
// .@.@.@.@@@
// @.@@@.@@@@
// .@@@@@@@@.
// @.@.@@@.@.
// The forklifts can only access a roll of paper if there are fewer than four rolls of paper in the eight adjacent positions. If you can figure out which rolls of paper the forklifts can access, they'll spend less time looking and more time breaking down the wall to the cafeteria.

// In this example, there are 13 rolls of paper that can be accessed by a forklift (marked with x):

// ..xx.xx@x.
// x@@.@.@.@@
// @@@@@.x.@@
// @.@@@@..@.
// x@.@@@@.@x
// .@@@@@@@.@
// .@.@.@.@@@
// x.@@@.@@@@
// .@@@@@@@@.
// x.x.@@@.x.
// Consider your complete diagram of the paper roll locations. How many rolls of paper can be accessed by a forklift?

let exampleLines = 
    [|
        "..@@.@@@@."
        "@@@.@.@.@@"
        "@@@@@.@.@@"
        "@.@@@@..@."
        "@@.@@@@.@@"
        ".@@@@@@@.@"
        ".@.@.@.@@@"
        "@.@@@.@@@@"
        ".@@@@@@@@."
        "@.@.@@@.@."
    |]

let lines = System.IO.File.ReadLines(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Day4.txt"))

// let lines = exampleLines

let dataLines = lines |> Seq.toArray

let width = dataLines[0].Length
let height = dataLines.Length
let up (x, y) = (x, y - 1)
let down (x, y) = (x, y + 1)
let left (x, y) = (x - 1, y)
let right (x, y) = (x + 1, y)
let upLeft (x, y) = (x - 1, y - 1)
let upRight (x, y) = (x + 1, y - 1)
let downLeft (x, y) = (x - 1, y + 1)
let downRight (x, y) = (x + 1, y + 1)
let inBounds (x, y) = x >= 0 && x < width && y >= 0 && y < height

let getChar (data: string[]) (x, y) =
    if inBounds (x, y)
    then Some(data[y][x])
    else None

let part1 =
    seq { 0 .. height - 1 }
    |> Seq.collect (fun y ->
        seq { 0 .. width - 1 }
        |> Seq.map (fun x -> 
            let ch = getChar dataLines (x, y) |> Option.defaultValue '.'
            if ch = '@' then
                let adjacentPositions = 
                    [ up (x, y); down (x, y); left (x, y); right (x, y);
                    upLeft (x, y); upRight (x, y); downLeft (x, y); downRight (x, y) ]

                let adjacentRolls =
                    adjacentPositions
                    |> List.choose (getChar dataLines)
                    |> List.filter (fun c -> c = '@')
                    |> List.length
                if adjacentRolls < 4 then
                    1
                else
                    0
            else
                0
        )
    )
    |> Seq.sum

// --- Part Two ---
// Now, the Elves just need help accessing as much of the paper as they can.

// Once a roll of paper can be accessed by a forklift, it can be removed. Once a roll of paper is removed, the forklifts might be able to access more rolls of paper, which they might also be able to remove. How many total rolls of paper could the Elves remove if they keep repeating this process?

// Starting with the same example as above, here is one way you could remove as many rolls of paper as possible, using highlighted @ to indicate that a roll of paper is about to be removed, and using x to indicate that a roll of paper was just removed:

// Initial state:
// ..@@.@@@@.
// @@@.@.@.@@
// @@@@@.@.@@
// @.@@@@..@.
// @@.@@@@.@@
// .@@@@@@@.@
// .@.@.@.@@@
// @.@@@.@@@@
// .@@@@@@@@.
// @.@.@@@.@.

// Remove 13 rolls of paper:
// ..xx.xx@x.
// x@@.@.@.@@
// @@@@@.x.@@
// @.@@@@..@.
// x@.@@@@.@x
// .@@@@@@@.@
// .@.@.@.@@@
// x.@@@.@@@@
// .@@@@@@@@.
// x.x.@@@.x.

// Remove 12 rolls of paper:
// .......x..
// .@@.x.x.@x
// x@@@@...@@
// x.@@@@..x.
// .@.@@@@.x.
// .x@@@@@@.x
// .x.@.@.@@@
// ..@@@.@@@@
// .x@@@@@@@.
// ....@@@...

// Remove 7 rolls of paper:
// ..........
// .x@.....x.
// .@@@@...xx
// ..@@@@....
// .x.@@@@...
// ..@@@@@@..
// ...@.@.@@x
// ..@@@.@@@@
// ..x@@@@@@.
// ....@@@...

// Remove 5 rolls of paper:
// ..........
// ..x.......
// .x@@@.....
// ..@@@@....
// ...@@@@...
// ..x@@@@@..
// ...@.@.@@.
// ..x@@.@@@x
// ...@@@@@@.
// ....@@@...

// Remove 2 rolls of paper:
// ..........
// ..........
// ..x@@.....
// ..@@@@....
// ...@@@@...
// ...@@@@@..
// ...@.@.@@.
// ...@@.@@@.
// ...@@@@@x.
// ....@@@...

// Remove 1 roll of paper:
// ..........
// ..........
// ...@@.....
// ..x@@@....
// ...@@@@...
// ...@@@@@..
// ...@.@.@@.
// ...@@.@@@.
// ...@@@@@..
// ....@@@...

// Remove 1 roll of paper:
// ..........
// ..........
// ...x@.....
// ...@@@....
// ...@@@@...
// ...@@@@@..
// ...@.@.@@.
// ...@@.@@@.
// ...@@@@@..
// ....@@@...

// Remove 1 roll of paper:
// ..........
// ..........
// ....x.....
// ...@@@....
// ...@@@@...
// ...@@@@@..
// ...@.@.@@.
// ...@@.@@@.
// ...@@@@@..
// ....@@@...

// Remove 1 roll of paper:
// ..........
// ..........
// ..........
// ...x@@....
// ...@@@@...
// ...@@@@@..
// ...@.@.@@.
// ...@@.@@@.
// ...@@@@@..
// ....@@@...
// Stop once no more rolls of paper are accessible by a forklift. In this example, a total of 43 rolls of paper can be removed.

// Start with your original diagram. How many rolls of paper in total can be removed by the Elves and their forklifts?

let removeAccessibleRolls d =
    seq { 0 .. height - 1 }
    |> Seq.map (fun y ->
        seq { 0 .. width - 1 }
        |> Seq.map (fun x -> 
            match getChar d (x, y) with
            | Some ch when ch = '@' ->
                let adjacentPositions = 
                    [ up (x, y); down (x, y); left (x, y); right (x, y);
                      upLeft (x, y); upRight (x, y); downLeft (x, y); downRight (x, y) ]

                let adjacentRolls =
                    adjacentPositions
                    |> List.choose (getChar d)
                    |> List.filter (fun c -> c = '@')
                    |> List.length
                
                if adjacentRolls < 4
                then 'x'
                else '@'
            | Some ch -> ch
            | None -> '.'
        )
        |> Seq.map string
        |> String.concat ""
    )
    |> Seq.toArray
    
let countRollsRemoved data =
    data
    |> Seq.collect (fun line ->
        line
        |> Seq.map (fun c -> if c = 'x' then 1 else 0)
    )
    |> Seq.sum

let part2 = 
    let mutable oldCount = -1
    let mutable removedCount = 0
    let mutable currentData = dataLines
    while removedCount <> oldCount do
        removedCount <- oldCount
        currentData <- removeAccessibleRolls currentData
        oldCount <- countRollsRemoved currentData
    oldCount
