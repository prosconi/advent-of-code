// --- Day 10: Pipe Maze ---
// You use the hang glider to ride the hot air from Desert Island all the way up to the floating metal island. This island is surprisingly cold and there definitely aren't any thermals to glide on, so you leave your hang glider behind.

// You wander around for a while, but you don't find any people or animals. However, you do occasionally find signposts labeled "Hot Springs" pointing in a seemingly consistent direction; maybe you can find someone at the hot springs and ask them where the desert-machine parts are made.

// The landscape here is alien; even the flowers and trees are made of metal. As you stop to admire some metal grass, you notice something metallic scurry away in your peripheral vision and jump into a big pipe! It didn't look like any animal you've ever seen; if you want a better look, you'll need to get ahead of it.

// Scanning the area, you discover that the entire field you're standing on is densely packed with pipes; it was hard to tell at first because they're the same metallic silver color as the "ground". You make a quick sketch of all of the surface pipes you can see (your puzzle input).

// The pipes are arranged in a two-dimensional grid of tiles:

// | is a vertical pipe connecting north and south.
// - is a horizontal pipe connecting east and west.
// L is a 90-degree bend connecting north and east.
// J is a 90-degree bend connecting north and west.
// 7 is a 90-degree bend connecting south and west.
// F is a 90-degree bend connecting south and east.
// . is ground; there is no pipe in this tile.
// S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
// Based on the acoustics of the animal's scurrying, you're confident the pipe that contains the animal is one large, continuous loop.

// For example, here is a square loop of pipe:

// .....
// .F-7.
// .|.|.
// .L-J.
// .....
// If the animal had entered this loop in the northwest corner, the sketch would instead look like this:

// .....
// .S-7.
// .|.|.
// .L-J.
// .....
// In the above diagram, the S tile is still a 90-degree F bend: you can tell because of how the adjacent pipes connect to it.

// Unfortunately, there are also many pipes that aren't connected to the loop! This sketch shows the same loop as above:

// -L|F7
// 7S-7|
// L|7||
// -L-J|
// L|-JF
// In the above diagram, you can still figure out which pipes form the main loop: they're the ones connected to S, pipes those pipes connect to, pipes those pipes connect to, and so on. Every pipe in the main loop connects to its two neighbors (including S, which will have exactly two pipes connecting to it, and which is assumed to connect back to those two pipes).

// Here is a sketch that contains a slightly more complex main loop:

// ..F7.
// .FJ|.
// SJ.L7
// |F--J
// LJ...
// Here's the same example sketch with the extra, non-main-loop pipe tiles also shown:

// 7-F7-
// .FJ|7
// SJLL7
// |F--J
// LJ.LJ
// If you want to get out ahead of the animal, you should find the tile in the loop that is farthest from the starting position. Because the animal is in the pipe, it doesn't make sense to measure this by direct distance. Instead, you need to find the tile that would take the longest number of steps along the loop to reach from the starting point - regardless of which way around the loop the animal went.

// In the first example with the square loop:

// .....
// .S-7.
// .|.|.
// .L-J.
// .....
// You can count the distance each tile in the loop is from the starting point like this:

// .....
// .012.
// .1.3.
// .234.
// .....
// In this example, the farthest point from the start is 4 steps away.

// Here's the more complex loop again:

// ..F7.
// .FJ|.
// SJ.L7
// |F--J
// LJ...
// Here are the distances for each tile on that loop:

// ..45.
// .236.
// 01.78
// 14567
// 23...
// Find the single giant loop starting at S. How many steps along the loop does it take to get from the starting position to the point farthest from the starting position?

open System
open System.IO

let example() =
    [|
        "....."
        ".S-7."
        ".|.|."
        ".L-J."
        "....."
    |]

let readInputFile() =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Day10.txt"))

type Tile =
    | Ground
    | NorthSouth
    | EastWest
    | NorthEast
    | NorthWest
    | SouthWest
    | SouthEast
    | Start
    
let charToTile c =
    match c with
    | '.' -> Ground
    | '|' -> NorthSouth
    | '-' -> EastWest
    | 'L' -> NorthEast
    | 'J' -> NorthWest
    | '7' -> SouthWest
    | 'F' -> SouthEast
    | 'S' -> Start
    | _ -> failwith "Unknown tile"

let parse (lines: string seq) =
    lines
    |> Seq.map (fun line -> line |> Seq.map charToTile |> Seq.toArray)
    |> Seq.toArray

let findStartingPosition tiles =
    tiles
    |> Array.mapi (fun y tile -> 
        tile
        |> Array.mapi (fun x tile -> (x, y), tile)
        |> Array.filter (fun (_, tile) -> tile = Start)
        |> Array.tryHead
    )
    |> Seq.choose id
    |> Seq.head

let solve (tiles: Tile[][]) =
    let solution = tiles |> Array.map (fun line -> line |> Array.map (fun _ -> Int32.MaxValue))
    let startingPosition, _ = findStartingPosition tiles
    let height = tiles.Length
    let width = tiles.[0].Length

    printfn "Starting position: %A" startingPosition

    let peak (x, y) =
        if x >= 0 && y >= 0 && x < width && y < height
        then Some(tiles.[y].[x])
        else None

    let rec traverse position previousPosition depth =

        let tif p =
            match peak p with
            | Some _ -> 
                if p <> previousPosition then
                    printfn "Traverse: %A, Depth: %d" p depth
                    traverse p position (depth + 1)
            | None ->
                ()

        let (x, y) = position

        solution.[y].[x] <- Math.Min(depth, solution.[y].[x])

        match tiles.[y].[x] with
        | Ground -> ()
        | NorthSouth -> 
            tif (x, y - 1)
            tif (x, y + 1)
        | EastWest -> 
            tif (x - 1, y)
            tif (x + 1, y)
        | NorthEast ->
            tif (x, y - 1)
            tif (x + 1, y)
        | NorthWest ->
            tif (x - 1, y)
            tif (x, y - 1)
        | SouthWest ->
            tif (x - 1, y)
            tif (x, y + 1)
        | SouthEast ->
            tif (x, y + 1)
            tif (x + 1, y)
        | Start ->
            if previousPosition = (-1, -1) then
                match peak (x, y - 1) with
                | Some NorthSouth
                | Some SouthEast
                | Some SouthWest -> tif (x, y - 1)
                | _ -> ()

                match peak (x, y + 1) with
                | Some NorthSouth
                | Some NorthEast
                | Some NorthWest -> tif (x, y + 1)
                | _ -> ()

                match peak (x - 1, y) with
                | Some EastWest
                | Some NorthEast
                | Some SouthEast -> tif (x - 1, y)
                | _ -> ()

                match peak (x + 1, y) with
                | Some EastWest
                | Some NorthWest
                | Some SouthWest -> tif (x + 1, y)
                | _ -> ()

    traverse startingPosition (-1, -1) 0
    solution

let tryMax arr =
    match arr with
    | [||] -> None
    | other -> Some(Array.max other)

readInputFile()
|> parse
|> solve
|> Array.choose (fun line -> line |> Array.filter (fun x -> x <> Int32.MaxValue) |> tryMax)
|> tryMax
