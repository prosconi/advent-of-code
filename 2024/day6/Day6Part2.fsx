// --- Part Two ---
// While The Historians begin working around the guard's patrol route, you borrow their fancy device and step outside the lab. From the safety of a supply closet, you time travel through the last few months and record the nightly status of the lab's guard post on the walls of the closet.

// Returning after what seems like only a few seconds to The Historians, they explain that the guard's patrol area is simply too large for them to safely search the lab without getting caught.

// Fortunately, they are pretty sure that adding a single new obstruction won't cause a time paradox. They'd like to place the new obstruction in such a way that the guard will get stuck in a loop, making the rest of the lab safe to search.

// To have the lowest chance of creating a time paradox, The Historians would like to know all of the possible positions for such an obstruction. The new obstruction can't be placed at the guard's starting position - the guard is there right now and would notice.

// In the above example, there are only 6 different positions where a new obstruction would cause the guard to get stuck in a loop. The diagrams of these six situations use O to mark the new obstruction, | to show a position where the guard moves up/down, - to show a position where the guard moves left/right, and + to show a position where the guard moves both up/down and left/right.

// Option one, put a printing press next to the guard's starting position:

// ....#.....
// ....+---+#
// ....|...|.
// ..#.|...|.
// ....|..#|.
// ....|...|.
// .#.O^---+.
// ........#.
// #.........
// ......#...
// Option two, put a stack of failed suit prototypes in the bottom right quadrant of the mapped area:


// ....#.....
// ....+---+#
// ....|...|.
// ..#.|...|.
// ..+-+-+#|.
// ..|.|.|.|.
// .#+-^-+-+.
// ......O.#.
// #.........
// ......#...
// Option three, put a crate of chimney-squeeze prototype fabric next to the standing desk in the bottom right quadrant:

// ....#.....
// ....+---+#
// ....|...|.
// ..#.|...|.
// ..+-+-+#|.
// ..|.|.|.|.
// .#+-^-+-+.
// .+----+O#.
// #+----+...
// ......#...
// Option four, put an alchemical retroencabulator near the bottom left corner:

// ....#.....
// ....+---+#
// ....|...|.
// ..#.|...|.
// ..+-+-+#|.
// ..|.|.|.|.
// .#+-^-+-+.
// ..|...|.#.
// #O+---+...
// ......#...
// Option five, put the alchemical retroencabulator a bit to the right instead:

// ....#.....
// ....+---+#
// ....|...|.
// ..#.|...|.
// ..+-+-+#|.
// ..|.|.|.|.
// .#+-^-+-+.
// ....|.|.#.
// #..O+-+...
// ......#...
// Option six, put a tank of sovereign glue right next to the tank of universal solvent:

// ....#.....
// ....+---+#
// ....|...|.
// ..#.|...|.
// ..+-+-+#|.
// ..|.|.|.|.
// .#+-^-+-+.
// .+----++#.
// #+----++..
// ......#O..
// It doesn't really matter what you choose to use as an obstacle so long as you and The Historians can put it into position without the guard noticing. The important thing is having enough options that you can find one that minimizes time paradoxes, and in this example, there are 6 different positions you could choose.

// You need to get the guard stuck in a loop by adding a single new obstruction. How many different positions could you choose for this obstruction?

open System.Collections.Generic
open System.IO

let exampleLines = 
    [|
        "....#....."
        ".........#"
        ".........."
        "..#......."
        ".......#.."
        ".........."
        ".#..^....."
        "........#."
        "#........."
        "......#..."
    |]

let lines = 
    Path.Combine(__SOURCE_DIRECTORY__, "Day6.txt")
    |> File.ReadAllLines

type Direction =
    | FacingNorth
    | FacingEast
    | FacingSouth
    | FacingWest

let ninetyDegreeRight = function
    | FacingNorth -> FacingEast
    | FacingEast -> FacingSouth
    | FacingSouth -> FacingWest
    | FacingWest -> FacingNorth

let data() = lines |> Array.map (fun x -> x.ToCharArray())
let originalData = data()

let up (x, y) = (x, y - 1)
let down (x, y) = (x, y + 1)
let left (x, y) = (x - 1, y)
let right (x, y) = (x + 1, y)

let charToDirection = function
    | '^' -> FacingNorth
    | '>' -> FacingEast
    | 'v' -> FacingSouth
    | '<' -> FacingWest
    | _ -> failwith "Invalid direction"

let findStartingPosition() =
    originalData
    |> Seq.mapi (fun y line ->
        line
        |> Seq.mapi (fun x c -> (x, y), c)
        |> Seq.tryFind (fun (_, c) -> 
            match c with
            | '>'
            | '<'
            | '^'
            | 'v' -> true
            | _ -> false
        )
    )
    |> Seq.choose id
    |> Seq.head

let startingPos, char = findStartingPosition()
let startingDirection = charToDirection char

let width = originalData[0].Length
let height = originalData.Length

let inBounds (x, y) = x >= 0 && x < width && y >= 0 && y < height
    
let getChar (data: char[][]) (x, y) =
    if inBounds (x, y)
    then Some(data[y][x])
    else None

let isObstacle c =
    c = '#' 

type CalcResult =
    | Loop
    | NoLoop

let calc data startingPos startingDir =

    let rec tick' (positionHistory: Dictionary<_,_>) position direction =
        let nextPosition =
            match direction with
            | FacingNorth -> up position
            | FacingEast -> right position
            | FacingSouth -> down position
            | FacingWest -> left position

        let key = position, direction
        match positionHistory.ContainsKey(key) with
        | true -> Loop
        | false ->
            positionHistory[key] <- true

            match getChar data nextPosition with
            | None -> NoLoop
            | Some c when isObstacle c -> 
                let newDirection = ninetyDegreeRight direction
                tick' positionHistory position newDirection
            | _ ->
                tick' positionHistory nextPosition direction

    let positionHistory = new Dictionary<_,_>()
    tick' positionHistory startingPos startingDir

let results =
    seq {
        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                match originalData[y][x] with
                | '.' -> 
                    let newData = data()
                    newData[y][x] <- '#'
                    calc newData startingPos startingDirection
                | _ -> NoLoop
    }
    |> Seq.filter (fun x -> x = Loop)
    |> Seq.length
