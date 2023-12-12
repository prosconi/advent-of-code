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

let floodFill position value (solution: int[][]) =
    let height = solution.Length
    let width = solution.[0].Length
    
    let rec floodFill' (x, y) =

        if solution[y][x] <> Int32.MaxValue then 
            ()
        else 
            solution[y][x] <- value

            if x > 0 then floodFill' (x - 1, y)
            if x < width - 1 then floodFill' (x + 1, y)
            if y > 0 then floodFill' (x, y - 1)
            if y < height - 1 then floodFill' (x, y + 1)

    floodFill' position

type Enclosed =
    | Pipe
    | Inside
    | Outside

let classify (x, y) (tiles: Tile[][]) (solution: int[][]) =
    let height = solution.Length
    let width = solution.[0].Length

    let isOnBorder(x', y') = x' = 0 || y' = 0 || x' = width - 1 || y' = height - 1

    if solution.[y].[x] <> Int32.MaxValue then Pipe
    else 
        if isOnBorder(x, y)
        then Outside
        else
            let mutable trappedRight = false
            let mutable xx = x
            while xx < width do
                if solution.[y].[xx] <> Int32.MaxValue then 
                    trappedRight <- true
                xx <- xx + 1

            let mutable trappedLeft = false
            let mutable xx = x
            while xx >= 0 do
                if solution.[y].[xx] <> Int32.MaxValue then
                    trappedLeft <- true
                xx <- xx - 1

            let mutable trappedDown = false
            let mutable yy = y
            while yy < height do
                if solution.[yy].[x] <> Int32.MaxValue then trappedDown <- true
                yy <- yy + 1

            let mutable trappedUp = false
            let mutable yy = y
            while yy >= 0 do
                if solution.[yy].[x] <> Int32.MaxValue then trappedUp <- true
                yy <- yy - 1

            if trappedDown && trappedUp && trappedLeft && trappedRight
            then Inside
            else Outside

let go (solution: int[][]) =
    for y = 0 to solution.Length - 1 do
        for x = 0 to solution.[0].Length - 1 do
            match classify (x, y) solution with
            | Pipe -> ()
            | Inside -> floodFill (x, y) -1 solution
            | Outside -> floodFill (x, y) -2 solution

let solution = 
    example4()
    |> parse
    |> solve
    
solution
|> go

solution
|> Array.map (fun line -> line |> Array.map (fun v -> if v = -1 then 'I' elif v = -2 then 'O' else ' ') |> Array.map string |> String.concat "")
|> Array.iter (printfn "%s")

solution
|> Array.collect (fun line -> line)
|> Array.map (fun x -> if x = -1 then 1 else 0)
|> Array.sum
