// --- Part Two ---
// You quickly reach the farthest point of the loop, but the animal never emerges. Maybe its nest is within the area enclosed by the loop?

// To determine whether it's even worth taking the time to search for such a nest, you should calculate how many tiles are contained within the loop. For example:

// ...........
// .S-------7.
// .|F-----7|.
// .||.....||.
// .||.....||.
// .|L-7.F-J|.
// .|..|.|..|.
// .L--J.L--J.
// ...........
// The above loop encloses merely four tiles - the two pairs of . in the southwest and southeast (marked I below). The middle . tiles (marked O below) are not in the loop. Here is the same loop again with those regions marked:

// ...........
// .S-------7.
// .|F-----7|.
// .||OOOOO||.
// .||OOOOO||.
// .|L-7OF-J|.
// .|II|O|II|.
// .L--JOL--J.
// .....O.....
// In fact, there doesn't even need to be a full tile path to the outside for tiles to count as outside the loop - squeezing between pipes is also allowed! Here, I is still within the loop and O is still outside the loop:

// ..........
// .S------7.
// .|F----7|.
// .||OOOO||.
// .||OOOO||.
// .|L-7F-J|.
// .|II||II|.
// .L--JL--J.
// ..........
// In both of the above examples, 4 tiles are enclosed by the loop.

// Here's a larger example:

// .F----7F7F7F7F-7....
// .|F--7||||||||FJ....
// .||.FJ||||||||L7....
// FJL7L7LJLJ||LJ.L-7..
// L--J.L7...LJS7F-7L7.
// ....F-J..F7FJ|L7L7L7
// ....L7.F7||L7|.L7L7|
// .....|FJLJ|FJ|F7|.LJ
// ....FJL-7.||.||||...
// ....L---J.LJ.LJLJ...
// The above sketch has many random bits of ground, some of which are in the loop (I) and some of which are outside it (O):

// OF----7F7F7F7F-7OOOO
// O|F--7||||||||FJOOOO
// O||OFJ||||||||L7OOOO
// FJL7L7LJLJ||LJIL-7OO
// L--JOL7IIILJS7F-7L7O
// OOOOF-JIIF7FJ|L7L7L7
// OOOOL7IF7||L7|IL7L7|
// OOOOO|FJLJ|FJ|F7|OLJ
// OOOOFJL-7O||O||||OOO
// OOOOL---JOLJOLJLJOOO
// In this larger example, 8 tiles are enclosed by the loop.

// Any tile that isn't part of the main loop can count as being enclosed by the loop. Here's another example with many bits of junk pipe lying around that aren't connected to the main loop at all:

// FF7FSF7F7F7F7F7F---7
// L|LJ||||||||||||F--J
// FL-7LJLJ||||||LJL-77
// F--JF--7||LJLJ7F7FJ-
// L---JF-JLJ.||-FJLJJ7
// |F|F-JF---7F7-L7L|7|
// |FFJF7L7F-JF7|JL---7
// 7-L-JL7||F7|L7F-7F7|
// L.L7LFJ|||||FJL7||LJ
// L7JLJL-JLJLJL--JLJ.L
// Here are just the tiles that are enclosed by the loop marked with I:

// FF7FSF7F7F7F7F7F---7
// L|LJ||||||||||||F--J
// FL-7LJLJ||||||LJL-77
// F--JF--7||LJLJIF7FJ-
// L---JF-JLJIIIIFJLJJ7
// |F|F-JF---7IIIL7L|7|
// |FFJF7L7F-JF7IIL---7
// 7-L-JL7||F7|L7F-7F7|
// L.L7LFJ|||||FJL7||LJ
// L7JLJL-JLJLJL--JLJ.L
// In this last example, 10 tiles are enclosed by the loop.

// Figure out whether you have time to search for the nest by calculating the area within the loop. How many tiles are enclosed by the loop?
 

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

let example2() =
    [|
        "..........."
        ".S-------7."
        ".|F-----7|."
        ".||.....||."
        ".||.....||."
        ".|L-7.F-J|."
        ".|..|.|..|."
        ".L--J.L--J."
        "..........."
    |]

let example3() =
    [|
        ".F----7F7F7F7F-7...."
        ".|F--7||||||||FJ...."
        ".||.FJ||||||||L7...."
        "FJL7L7LJLJ||LJ.L-7.."
        "L--J.L7...LJS7F-7L7."
        "....F-J..F7FJ|L7L7L7"
        "....L7.F7||L7|.L7L7|"
        ".....|FJLJ|FJ|F7|.LJ"
        "....FJL-7.||.||||..."
        "....L---J.LJ.LJLJ..."
    |]

let example4() =
    [|
        ".........."
        ".S------7."
        ".|F----7|."
        ".||....||."
        ".||....||."
        ".|L-7F-J|."
        ".|..||..|."
        ".L--JL--J."
        ".........."
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
    member x.ToChar() =
        match x with
        | Ground -> '.'
        | NorthSouth -> '|'
        | EastWest -> '-'
        | NorthEast -> 'L'
        | NorthWest -> 'J'
        | SouthWest -> '7'
        | SouthEast -> 'F'
        | Start -> 'S'
    
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

let replaceStartingPosition tiles =
    let (x, y), _ = findStartingPosition tiles
    let height = tiles.Length
    let width = tiles.[0].Length
    let peak (x, y) =
        if x >= 0 && y >= 0 && x < width && y < height
        then Some(tiles.[y].[x])
        else None

    let isNorth = 
        match peak (x, y - 1) with
        | Some NorthSouth
        | Some SouthEast
        | Some SouthWest -> true
        | _ -> false

    let isSouth =
        match peak (x, y + 1) with
        | Some NorthSouth
        | Some NorthEast
        | Some NorthWest -> true
        | _ -> false

    let isWest =
        match peak (x - 1, y) with
        | Some EastWest
        | Some NorthEast
        | Some SouthEast -> true
        | _ -> false

    let isEast =
        match peak (x + 1, y) with
        | Some EastWest
        | Some NorthWest
        | Some SouthWest -> true
        | _ -> false

    let newTile = 
        match isEast, isWest, isNorth, isSouth with
        | true, true, _, _ -> EastWest
        | _, _, true, true -> NorthSouth
        | true, _, true, _ -> NorthEast
        | _, true, true, _ -> NorthWest
        | _, true, _, true -> SouthWest
        | true, _, _, true -> SouthEast
        | _ -> failwith "Invalid starting position"

    tiles.[y].[x] <- newTile
    x, y

let solve startingPosition (tiles: Tile[][]) =
    
    let solution = tiles |> Array.map (fun line -> line |> Array.map (fun _ -> Int32.MaxValue))
    let height = tiles.Length
    let width = tiles.[0].Length

    let peak (x, y) =
        if x >= 0 && y >= 0 && x < width && y < height
        then Some(tiles.[y].[x])
        else None

    let rec traverse position previousPosition depth =

        let tif p =
            match peak p with
            | Some _ -> 
                if p <> previousPosition then
                    traverse p position (depth + 1)
            | None ->
                ()

        // stop from recursing forever (we are at start position)
        if position = startingPosition && previousPosition <> (-1, -1) then
            ()
        else
            let (x, y) = position
            solution.[y].[x] <- Math.Min(depth, solution.[y].[x])

            match tiles.[y].[x] with
            | Ground -> ()
            | Start -> ()
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

    traverse startingPosition (-1, -1) 0
    solution


let tiles = readInputFile() |> parse

let startingPosition = replaceStartingPosition tiles

tiles

let solution = solve startingPosition tiles

let tryMax arr =
    match arr with
    | [||] -> None
    | other -> Some(Array.max other)

solution
|> Array.choose (fun line -> line |> Array.filter (fun x -> x <> Int32.MaxValue) |> tryMax)
|> tryMax

tiles
|> Array.map (fun line -> line |> Array.map (fun x -> x.ToChar().ToString()))
|> Array.iter (fun line -> line |> Array.iter (printf "%s"); printfn "")

