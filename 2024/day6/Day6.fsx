// --- Day 6: Guard Gallivant ---
// The Historians use their fancy device again, this time to whisk you all away to the North Pole prototype suit manufacturing lab... in the year 1518! It turns out that having direct access to history is very convenient for a group of historians.

// You still have to be careful of time paradoxes, and so it will be important to avoid anyone from 1518 while The Historians search for the Chief. Unfortunately, a single guard is patrolling this part of the lab.

// Maybe you can work out where the guard will go ahead of time so that The Historians can search safely?

// You start by making a map (your puzzle input) of the situation. For example:

// ....#.....
// .........#
// ..........
// ..#.......
// .......#..
// ..........
// .#..^.....
// ........#.
// #.........
// ......#...
// The map shows the current position of the guard with ^ (to indicate the guard is currently facing up from the perspective of the map). Any obstructions - crates, desks, alchemical reactors, etc. - are shown as #.

// Lab guards in 1518 follow a very strict patrol protocol which involves repeatedly following these steps:

// If there is something directly in front of you, turn right 90 degrees.
// Otherwise, take a step forward.
// Following the above protocol, the guard moves up several times until she reaches an obstacle (in this case, a pile of failed suit prototypes):

// ....#.....
// ....^....#
// ..........
// ..#.......
// .......#..
// ..........
// .#........
// ........#.
// #.........
// ......#...
// Because there is now an obstacle in front of the guard, she turns right before continuing straight in her new facing direction:

// ....#.....
// ........>#
// ..........
// ..#.......
// .......#..
// ..........
// .#........
// ........#.
// #.........
// ......#...
// Reaching another obstacle (a spool of several very long polymers), she turns right again and continues downward:

// ....#.....
// .........#
// ..........
// ..#.......
// .......#..
// ..........
// .#......v.
// ........#.
// #.........
// ......#...
// This process continues for a while, but the guard eventually leaves the mapped area (after walking past a tank of universal solvent):

// ....#.....
// .........#
// ..........
// ..#.......
// .......#..
// ..........
// .#........
// ........#.
// #.........
// ......#v..
// By predicting the guard's route, you can determine which specific positions in the lab will be in the patrol path. Including the guard's starting position, the positions visited by the guard before leaving the area are marked with an X:

// ....#.....
// ....XXXXX#
// ....X...X.
// ..#.X...X.
// ..XXXXX#X.
// ..X.X.X.X.
// .#XXXXXXX.
// .XXXXXXX#.
// #XXXXXXX..
// ......#X..
// In this example, the guard will visit 41 distinct positions on your map.

// Predict the path of the guard. How many distinct positions will the guard visit before leaving the mapped area?
open System
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

let data = lines

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
    data
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

let width = data[0].Length
let height = data.Length

let inBounds (x, y) = x >= 0 && x < width && y >= 0 && y < height
    
let getChar (x, y) =
    if inBounds (x, y)
    then Some(data[y][x])
    else None

let isObstacle c =
    c = '#' 

let calc startingPos startingDir =
    let rec tick' (positionHistory: Dictionary<_,_>) position direction =
        let nextPosition =
            match direction with
            | FacingNorth -> up position
            | FacingEast -> right position
            | FacingSouth -> down position
            | FacingWest -> left position

        positionHistory[position] <- true

        match getChar nextPosition with
        | None -> positionHistory
        | Some c when isObstacle c -> 
            let newDirection = ninetyDegreeRight direction
            tick' positionHistory position newDirection
        | _ -> tick' positionHistory nextPosition direction

    let positionHistory = new Dictionary<_,_>()
    tick' positionHistory startingPos startingDir

let positions = calc startingPos startingDirection
positions.Count
