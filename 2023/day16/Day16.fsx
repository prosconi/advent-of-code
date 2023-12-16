// --- Day 16: The Floor Will Be Lava ---
// With the beam of light completely focused somewhere, the reindeer leads you deeper still into the Lava Production Facility. At some point, you realize that the steel facility walls have been replaced with cave, and the doorways are just cave, and the floor is cave, and you're pretty sure this is actually just a giant cave.

// Finally, as you approach what must be the heart of the mountain, you see a bright light in a cavern up ahead. There, you discover that the beam of light you so carefully focused is emerging from the cavern wall closest to the facility and pouring all of its energy into a contraption on the opposite side.

// Upon closer inspection, the contraption appears to be a flat, two-dimensional square grid containing empty space (.), mirrors (/ and \), and splitters (| and -).

// The contraption is aligned so that most of the beam bounces around the grid, but each tile on the grid converts some of the beam's light into heat to melt the rock in the cavern.

// You note the layout of the contraption (your puzzle input). For example:

// .|...\....
// |.-.\.....
// .....|-...
// ........|.
// ..........
// .........\
// ..../.\\..
// .-.-/..|..
// .|....-|.\
// ..//.|....
// The beam enters in the top-left corner from the left and heading to the right. Then, its behavior depends on what it encounters as it moves:

// If the beam encounters empty space (.), it continues in the same direction.
// If the beam encounters a mirror (/ or \), the beam is reflected 90 degrees depending on the angle of the mirror. For instance, a rightward-moving beam that encounters a / mirror would continue upward in the mirror's column, while a rightward-moving beam that encounters a \ mirror would continue downward from the mirror's column.
// If the beam encounters the pointy end of a splitter (| or -), the beam passes through the splitter as if the splitter were empty space. For instance, a rightward-moving beam that encounters a - splitter would continue in the same direction.
// If the beam encounters the flat side of a splitter (| or -), the beam is split into two beams going in each of the two directions the splitter's pointy ends are pointing. For instance, a rightward-moving beam that encounters a | splitter would split into two beams: one that continues upward from the splitter's column and one that continues downward from the splitter's column.
// Beams do not interact with other beams; a tile can have many beams passing through it at the same time. A tile is energized if that tile has at least one beam pass through it, reflect in it, or split in it.

// In the above example, here is how the beam of light bounces around the contraption:

// >|<<<\....
// |v-.\^....
// .v...|->>>
// .v...v^.|.
// .v...v^...
// .v...v^..\
// .v../2\\..
// <->-/vv|..
// .|<<<2-|.\
// .v//.|.v..
// Beams are only shown on empty tiles; arrows indicate the direction of the beams. If a tile contains beams moving in multiple directions, the number of distinct directions is shown instead. Here is the same diagram but instead only showing whether a tile is energized (#) or not (.):

// ######....
// .#...#....
// .#...#####
// .#...##...
// .#...##...
// .#...##...
// .#..####..
// ########..
// .#######..
// .#...#.#..
// Ultimately, in this example, 46 tiles become energized.

// The light isn't energizing enough tiles to produce lava; to debug the contraption, you need to start by analyzing the current situation. With the beam starting in the top-left heading right, how many tiles end up being energized?

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

let start lines =

    let energized = lines |> Array.map (fun line -> Array.create (Array.length line) false)
    let mirrorHit = lines |> Array.map (fun line -> Array.create (Array.length line) { WasMovingWest = false; WasMovingEast = false; WasMovingNorth = false; WasMovingSouth = false} )
    let width = 0 |> Array.get lines |> Array.length
    let height = Array.length lines
    
    let rec go (x, y) (dx, dy) (lines: char[][]) =
        // System.Threading.Thread.Sleep(500)
        // System.Console.Clear()
        
        // energized
        // |> Array.iteri (fun yy line -> 
        //     line
        //     |> Array.mapi (fun xx isEnergized -> 
        //         if yy = y && xx = x then "X"
        //         elif lines[yy][xx] = '.' then if isEnergized then "#" else "."
        //         else string (lines[yy][xx])
        //     )
        //     |> Array.iter (fun y -> printf "%s" y)

        //     printfn ""
        // )

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
            printfn "%A - %A - %A" c (x, y) (dx, dy)
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

    go (0, 0) (1, 0) lines
    energized

readInputFile()
|> start
|> Array.collect (fun x -> x |> Array.map (fun y -> if y then 1 else 0))
|> Array.sum
