// --- Part Two ---
// The parabolic reflector dish deforms, but not in a way that focuses the beam. To do that, you'll need to move the rocks to the edges of the platform. Fortunately, a button on the side of the control panel labeled "spin cycle" attempts to do just that!

// Each cycle tilts the platform four times so that the rounded rocks roll north, then west, then south, then east. After each tilt, the rounded rocks roll as far as they can before the platform tilts in the next direction. After one cycle, the platform will have finished rolling the rounded rocks in those four directions in that order.

// Here's what happens in the example above after each of the first few cycles:

// After 1 cycle:
// .....#....
// ....#...O#
// ...OO##...
// .OO#......
// .....OOO#.
// .O#...O#.#
// ....O#....
// ......OOOO
// #...O###..
// #..OO#....

// After 2 cycles:
// .....#....
// ....#...O#
// .....##...
// ..O#......
// .....OOO#.
// .O#...O#.#
// ....O#...O
// .......OOO
// #..OO###..
// #.OOO#...O

// After 3 cycles:
// .....#....
// ....#...O#
// .....##...
// ..O#......
// .....OOO#.
// .O#...O#.#
// ....O#...O
// .......OOO
// #...O###.O
// #.OOO#...O
// This process should work if you leave it running long enough, but you're still worried about the north support beams. To make sure they'll survive for a while, you need to calculate the total load on the north support beams after 1000000000 cycles.

// In the above example, after 1000000000 cycles, the total load on the north support beams is 64.

// Run the spin cycle for 1000000000 cycles. Afterward, what is the total load on the north support beams?

open System.IO

let readInputFile() =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Day14.txt"))
    |> Seq.map (fun x -> x.ToCharArray())
    |> Seq.toArray

let example() =
    [|
        "O....#...."
        "O.OO#....#"
        ".....##..."
        "OO.#O....O"
        ".O.....O#."
        "O.#..O.#.#"
        "..O..#O..O"
        ".......O.."
        "#....###.."
        "#OO..#...."
    |]
    |> Array.map (fun x -> x.ToCharArray())

let rec moveV dy (lines: char[][]) x y =
    if y + dy < 0 || y + dy > lines.Length - 1
    then ()
    else
        match lines.[y + dy].[x] with
        | '.' -> 
            lines.[y].[x] <- '.'
            lines.[y + dy][x] <- 'O'
            moveV dy lines x (y + dy)
        | _ -> ()

let rec moveH dx (lines: char[][]) x y =
    if x + dx < 0 || x + dx > lines[0].Length - 1
    then ()
    else 
        match lines.[y].[x + dx] with
        | '.' -> 
            lines.[y].[x] <- '.'
            lines.[y].[x + dx] <- 'O'
            moveH dx lines (x + dx) y
        | _ -> ()

let rec moveEast = moveH 1
let rec moveWest = moveH -1
let rec moveSouth = moveV 1
let rec moveNorth = moveV -1

let copyMatrix lines =
    Array.map (fun x -> Array.copy x) lines

let moveEverythingNorth (lines: char[][]) =
    let lines = copyMatrix lines
    for y = 0 to lines.Length - 1 do
        for x = 0 to lines[y].Length - 1 do
            match lines[y][x] with
            | 'O' -> moveNorth lines x y
            | _ -> ()
    lines

let moveEverythingSouth (lines: char[][]) =
    let lines = copyMatrix lines
    for y = lines.Length - 1 downto 0 do
        for x = 0 to lines[y].Length - 1 do
            match lines[y][x] with
            | 'O' -> moveSouth lines x y
            | _ -> ()
    lines

let moveEverythingEast (lines: char[][]) =
    let lines = copyMatrix lines
    for y = 0 to lines.Length - 1 do
        for x = lines[y].Length - 1 downto 0 do
            match lines[y][x] with
            | 'O' -> moveEast lines x y
            | _ -> ()
    lines

let moveEverythingWest (lines: char[][]) =
    let lines = copyMatrix lines
    for y = 0 to lines.Length - 1 do
        for x = 0 to lines[y].Length - 1 do
            match lines[y][x] with
            | 'O' -> moveWest lines x y
            | _ -> ()
    lines

let calculateLoad (lines: char[][]) =
    seq {
        for y = 0 to lines.Length - 1 do
            for x = 0 to lines[y].Length - 1 do
                match lines[y][x] with
                | 'O' -> lines.Length - y
                | _ -> ()
    }
    |> Seq.sum

let memoize (f: 'a -> 'b) =
    let cache = System.Collections.Generic.Dictionary<'a, 'b>()
    fun (arg: 'a) ->
        match cache.TryGetValue(arg) with
        | true, value -> value
        | false, _ ->
            let result = f arg
            cache.Add(arg, result)
            result

let strToCharArray (s: string) =
    s.Split('\n') |> Seq.map (fun x -> x.ToCharArray())
    |> Seq.toArray

let moveMemoize = memoize (fun (lines: string) -> 
    lines
    |> strToCharArray
    |> moveEverythingNorth
    |> moveEverythingWest
    |> moveEverythingSouth
    |> moveEverythingEast
)

let stringInstances = ResizeArray()

let moveEverythingWithCycles cycleCount (lines: char[][]) =
    let mutable lines = lines
    let asString (l: char[][]) =
        l
        |> Array.map (fun x -> System.String(x))
        |> String.concat "\n"

    for i = 0 to cycleCount do
        lines <- 
            lines
            |> asString
            |> moveMemoize

        let s = asString lines
        let idx = stringInstances.IndexOf s
        if idx >= 0
        then printfn "Found %d at index: %d" i idx
        else stringInstances.Add s

        if i % 100_000 = 0 then 
            printfn "[%d] Cycle - %A" i (float i / float cycleCount)
        ()

    lines

// run this for awhile, then stop it
let result =
    readInputFile()
    |> moveEverythingWithCycles 1_000_000_000

// analyze the data, do some math... too lazy to make this programmatic
let cycleLength = 158 - 82
let calc index = 82 + (index - 20102) % (cycleLength + 1)
20102 = 82
20178 = 158  
20179 = 82

calc (1_000_000_000 - 1)

stringInstances[152]
|> strToCharArray
|> calculateLoad
