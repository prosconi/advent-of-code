// --- Day 17: Clumsy Crucible ---
// The lava starts flowing rapidly once the Lava Production Facility is operational. As you leave, the reindeer offers you a parachute, allowing you to quickly reach Gear Island.

// As you descend, your bird's-eye view of Gear Island reveals why you had trouble finding anyone on your way up: half of Gear Island is empty, but the half below you is a giant factory city!

// You land near the gradually-filling pool of lava at the base of your new lavafall. Lavaducts will eventually carry the lava throughout the city, but to make use of it immediately, Elves are loading it into large crucibles on wheels.

// The crucibles are top-heavy and pushed by hand. Unfortunately, the crucibles become very difficult to steer at high speeds, and so it can be hard to go in a straight line for very long.

// To get Desert Island the machine parts it needs as soon as possible, you'll need to find the best way to get the crucible from the lava pool to the machine parts factory. To do this, you need to minimize heat loss while choosing a route that doesn't require the crucible to go in a straight line for too long.

// Fortunately, the Elves here have a map (your puzzle input) that uses traffic patterns, ambient temperature, and hundreds of other parameters to calculate exactly how much heat loss can be expected for a crucible entering any particular city block.

// For example:

// 2413432311323
// 3215453535623
// 3255245654254
// 3446585845452
// 4546657867536
// 1438598798454
// 4457876987766
// 3637877979653
// 4654967986887
// 4564679986453
// 1224686865563
// 2546548887735
// 4322674655533
// Each city block is marked by a single digit that represents the amount of heat loss if the crucible enters that block. The starting point, the lava pool, is the top-left city block; the destination, the machine parts factory, is the bottom-right city block. (Because you already start in the top-left block, you don't incur that block's heat loss unless you leave that block and then return to it.)

// Because it is difficult to keep the top-heavy crucible going in a straight line for very long, it can move at most three blocks in a single direction before it must turn 90 degrees left or right. The crucible also can't reverse direction; after entering each city block, it may only turn left, continue straight, or turn right.

// One way to minimize heat loss is this path:

// 2>>34^>>>1323
// 32v>>>35v5623
// 32552456v>>54
// 3446585845v52
// 4546657867v>6
// 14385987984v4
// 44578769877v6
// 36378779796v>
// 465496798688v
// 456467998645v
// 12246868655<v
// 25465488877v5
// 43226746555v>
// This path never moves more than three consecutive blocks in the same direction and incurs a heat loss of only 102.

// Directing the crucible from the lava pool to the machine parts factory, but not moving more than three consecutive blocks in the same direction, what is the least heat loss it can incur?

open System
open System.IO

let readInputFile() =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Day17.txt"))
    |> Seq.map (fun s -> s.ToCharArray() |> Array.map (string >> Int32.Parse))
    |> Seq.toArray


let example() =
    [|
        "2413432311323"
        "3215453535623"
        "3255245654254"
        "3446585845452"
        "4546657867536"
        "1438598798454"
        "4457876987766"
        "3637877979653"
        "4654967986887"
        "4564679986453"
        "1224686865563"
        "2546548887735"
        "4322674655533"
    |]
    |> Seq.map (fun s -> s.ToCharArray() |> Array.map (string >> Int32.Parse))
    |> Seq.toArray

type Position = int * int
type Velocity = int * int

type Crucible =
    {
        mutable Position: Position
        mutable Velocity: Velocity
        mutable HeatLoss: int
        mutable Combo: int
        mutable Path: ResizeArray<Position>
        Color: ConsoleColor
    }

let randomConsoleColor() =
    let rnd = Random()
    let colors = Enum.GetValues(typeof<ConsoleColor>)
    colors.GetValue(rnd.Next(colors.Length)) :?> ConsoleColor

let simulate (area: int[][]) =
    let mins = 
        area
        |> Array.map (fun row -> row |> Array.map (fun _ -> Int32.MaxValue))

    let NORTH = (0, -1)
    let SOUTH = (0, 1)
    let EAST = (1, 0)
    let WEST = (-1, 0)

    let width = area.[0].Length
    let height = area.Length

    let completed = ResizeArray()
    let crucibles = ResizeArray()
    crucibles.Add({ Position = (0, 0); Velocity = (0, 1); HeatLoss = 0; Combo = 0; Color = randomConsoleColor(); Path = ResizeArray() })

    let add (c: Crucible) =
        let x, y = c.Position
        if x >= 0 && x < width && y >= 0 && y < height then // out of bounds, die
            c.HeatLoss <- c.HeatLoss + area.[y].[x]
            match crucibles |> Seq.tryFind (fun x -> x.Position = c.Position && x.Velocity = c.Velocity && x.HeatLoss <= x.HeatLoss) with
            | None -> 
                if c.HeatLoss < mins[y][x] then
                    crucibles.Add(c)
            | _ -> ()
        
    let move (c: Crucible) =
    
        let x, y = c.Position
        let dx, dy = c.Velocity

        if x < 0 || x >= width || y < 0 || y >= height then // out of bounds, die
            c |> crucibles.Remove |> ignore
        elif x = width - 1 && y = height - 1 then // reached destination, die
            c |> crucibles.Remove |> ignore
            c |> completed.Add
        elif c.HeatLoss >= mins[y][x] then // already been here, die
            c |> crucibles.Remove |> ignore
        else // move forward, left, and right

            c.Path.Add(c.Position)
            mins.[y].[x] <- Math.Min(c.HeatLoss, mins[y][x])

            // spawn new crucibles going left and right
            if c.Velocity = NORTH || c.Velocity = SOUTH then
                add({ Position = x + 1, y; Velocity = EAST; HeatLoss = c.HeatLoss; Combo = 0; Color = randomConsoleColor(); Path = ResizeArray(c.Path) })
                add({ Position = x - 1, y; Velocity = WEST; HeatLoss = c.HeatLoss; Combo = 0; Color = randomConsoleColor(); Path = ResizeArray(c.Path) })
            elif c.Velocity = WEST || c.Velocity = EAST then
                add({ Position = x, y - 1; Velocity = NORTH; HeatLoss = c.HeatLoss; Combo = 0; Color = randomConsoleColor(); Path = ResizeArray(c.Path) })
                add({ Position = x, y + 1; Velocity = SOUTH; HeatLoss = c.HeatLoss; Combo = 0; Color = randomConsoleColor(); Path = ResizeArray(c.Path) })

            // go straight
            if c.Combo < 3 then
                c.Position <- (x + dx, y + dy)
                c.Combo <- c.Combo + 1
                c.HeatLoss <- c.HeatLoss + area[y][x]
            else
                c |> crucibles.Remove |> ignore

    let draw() =
        Console.Clear()
        printfn "Crucibles: %d" crucibles.Count

        for y = 0 to height - 1 do
            for x = 0 to width - 1 do
                Console.SetCursorPosition(x + 20, y + 1)
                if mins.[y].[x] = Int32.MaxValue then
                    Console.Write(" ")
                else
                    Console.Write(".")

        for c in crucibles do
            let x, y = c.Position
            if x >= 0 && x < width && y >= 0 && y < height then
                Console.ForegroundColor <- c.Color
                Console.SetCursorPosition(x, y + 1)
                Console.Write("C")
                Console.ResetColor()

        Console.SetCursorPosition(0, 0)

    while crucibles.Count > 0 do
        // draw()
        for c in crucibles |> Seq.toArray do
            move c
        // Threading.Thread.Sleep(100)
    
    completed

let completed =
    readInputFile()
    |> simulate

completed
|> Seq.map (fun x -> x.HeatLoss)
|> Seq.min
