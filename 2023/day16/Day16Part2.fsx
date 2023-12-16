// --- Part Two ---
// As you try to work out what might be wrong, the reindeer tugs on your shirt and leads you to a nearby control panel. There, a collection of buttons lets you align the contraption so that the beam enters from any edge tile and heading away from that edge. (You can choose either of two directions for the beam if it starts on a corner; for instance, if the beam starts in the bottom-right corner, it can start heading either left or upward.)

// So, the beam could start on any tile in the top row (heading downward), any tile in the bottom row (heading upward), any tile in the leftmost column (heading right), or any tile in the rightmost column (heading left). To produce lava, you need to find the configuration that energizes as many tiles as possible.

// In the above example, this can be achieved by starting the beam in the fourth tile from the left in the top row:

// .|<2<\....
// |v-v\^....
// .v.v.|->>>
// .v.v.v^.|.
// .v.v.v^...
// .v.v.v^..\
// .v.v/2\\..
// <-2-/vv|..
// .|<<<2-|.\
// .v//.|.v..
// Using this configuration, 51 tiles are energized:

// .#####....
// .#.#.#....
// .#.#.#####
// .#.#.##...
// .#.#.##...
// .#.#.##...
// .#.#####..
// ########..
// .#######..
// .#...#.#..
// Find the initial beam configuration that energizes the largest number of tiles; how many tiles are energized in that configuration?

open System.IO

let readInputFile() =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Day16.txt"))
    |> Seq.map (fun s -> s.ToCharArray())
    |> Seq.toArray

let example() =
    [|
        @".|...\...."
        @"|.-.\....."
        @".....|-..."
        @"........|."
        @".........."
        @".........\"
        @"..../.\\.."
        @".-.-/..|.."
        @".|....-|.\"
        @"..//.|...."
    |]
    |> Array.map (fun s -> s.ToCharArray())

type MirrorHit =
    {
        WasMovingWest: bool
        WasMovingEast: bool
        WasMovingNorth: bool
        WasMovingSouth: bool
    }

let start startPos startVel lines =

    let energized = lines |> Array.map (fun line -> Array.create (Array.length line) false)
    let mirrorHit = lines |> Array.map (fun line -> Array.create (Array.length line) { WasMovingWest = false; WasMovingEast = false; WasMovingNorth = false; WasMovingSouth = false} )
    let width = 0 |> Array.get lines |> Array.length
    let height = Array.length lines
    
    let rec go (x, y) (dx, dy) (lines: char[][]) =
        let isMovingWest() = dx < 0
        let isMovingEast() = dx > 0
        let isMovingNorth() = dy < 0
        let isMovingSouth() = dy > 0

        if x < 0 || y < 0 || x >= width || y >= height then ()
        elif isMovingWest() && mirrorHit.[y].[x].WasMovingWest then ()
        elif isMovingEast() && mirrorHit.[y].[x].WasMovingEast then ()
        elif isMovingSouth() && mirrorHit.[y].[x].WasMovingSouth then ()
        elif isMovingNorth() && mirrorHit.[y].[x].WasMovingNorth then ()
        else
            energized[y].[x] <- true

            let c = lines.[y].[x]
            match c with
            | '.' ->
                go (x + dx, y + dy) (dx, dy) lines
            | '/' ->
                if isMovingEast() then
                    mirrorHit[y][x] <- { mirrorHit[y][x] with WasMovingEast = true }
                    go (x, y - 1) (0, -1) lines
                elif isMovingWest() then
                    mirrorHit[y][x] <- { mirrorHit[y][x] with WasMovingWest = true }
                    go (x, y + 1) (0, 1) lines
                elif isMovingNorth() then
                    mirrorHit[y][x] <- { mirrorHit[y][x] with WasMovingNorth = true }
                    go (x + 1, y) (1, 0) lines
                elif isMovingSouth() then
                    mirrorHit[y][x] <- { mirrorHit[y][x] with WasMovingSouth = true }
                    go (x - 1, y) (-1, 0) lines
            | '\\' -> 
                if isMovingEast() then
                    mirrorHit[y][x] <- { mirrorHit[y][x] with WasMovingEast = true }
                    go (x, y + 1) (0, 1) lines
                elif isMovingWest() then
                    mirrorHit[y][x] <- { mirrorHit[y][x] with WasMovingWest = true }
                    go (x, y - 1) (0, -1) lines
                elif isMovingNorth() then
                    mirrorHit[y][x] <- { mirrorHit[y][x] with WasMovingNorth = true }
                    go (x - 1, y) (-1, 0) lines
                elif isMovingSouth() then
                    mirrorHit[y][x] <- { mirrorHit[y][x] with WasMovingSouth = true }
                    go (x + 1, y) (1, 0) lines
            | '|' -> 
                if isMovingEast() || isMovingWest() then
                    mirrorHit[y][x] <- { WasMovingEast = true; WasMovingNorth = true; WasMovingSouth = true; WasMovingWest = true }
                    go (x, y + 1) (0, 1) lines
                    go (x, y - 1) (0, -1) lines
                else
                    go (x + dx, y + dy) (dx, dy) lines
            | '-' -> 
                if isMovingNorth() || isMovingSouth() then
                    mirrorHit[y][x] <- { WasMovingEast = true; WasMovingNorth = true; WasMovingSouth = true; WasMovingWest = true }
                    go (x + 1, y) (1, 0) lines
                    go (x - 1, y) (-1, 0) lines
                else
                    go (x + dx, y + dy) (dx, dy) lines
            | _ ->
                failwith "invalid input"

    go startPos startVel lines
    energized

let allPermuations lines =
    seq {
        let width = 0 |> Array.get lines |> Array.length
        let height = lines |> Array.length
        
        for y = 0 to height - 1 do
            start (0, y) (1, 0) lines
            start (width - 1, y) (-1, 0) lines

        for x = 0 to width - 1 do
            start (x, 0) (0, 1) lines
            start (x, height - 1) (0, -1) lines
    }

readInputFile()
|> allPermuations
|> Seq.map (fun x ->
    x
    |> Seq.collect (fun x -> x |> Seq.map (fun y -> if y then 1 else 0))
    |> Seq.sum
)
|> Seq.max

