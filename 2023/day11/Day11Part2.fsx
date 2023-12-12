// --- Part Two ---
// The galaxies are much older (and thus much farther apart) than the researcher initially estimated.

// Now, instead of the expansion you did before, make each empty row or column one million times larger. That is, each empty row should be replaced with 1000000 empty rows, and each empty column should be replaced with 1000000 empty columns.

// (In the example above, if each empty row or column were merely 10 times larger, the sum of the shortest paths between every pair of galaxies would be 1030. If each empty row or column were merely 100 times larger, the sum of the shortest paths between every pair of galaxies would be 8410. However, your universe will need to expand far beyond these values.)

// Starting with the same initial image, expand the universe according to these new rules, then find the length of the shortest path between every pair of galaxies. What is the sum of these lengths?

open System.IO

let example() =
    [|
        "...#......"
        ".......#.."
        "#........."
        ".........."
        "......#..."
        ".#........"
        ".........#"
        ".........."
        ".......#.."
        "#...#....."
    |]

let readInputFile() =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Day11.txt"))

type ExpandedUniverse =
    {
        Universe: string[]
        RowExpansions: int[]
        ColExpansions: int[]
    }

let expandUniverse (universe: string[]) =

    let width = universe[0].Length
    let str = String.replicate width "."

    let rowExpansions =
        [|
            for y = universe.Length - 1 downto 0 do
                let row = universe.[y]
                if row = str then yield y
        |]

    let colExpansions =
        [|
            for x = width - 1 downto 0 do
                if [| for y = 0 to universe.Length - 1 do universe[y][x] |] |> Seq.forall ((=) '.') then yield x             
        |]

    { Universe = universe; RowExpansions = rowExpansions; ColExpansions = colExpansions }

let getOffsetPosition expansionSize (eu: ExpandedUniverse) (x, y) =
    let colOffset =
        eu.ColExpansions
        |> Seq.filter (fun xx -> x > xx)
        |> Seq.length

    let rowOffset =
        eu.RowExpansions
        |> Seq.filter (fun yy -> y > yy)
        |> Seq.length

    printfn "x: %d, y: %d, colOffset: %d, rowOffset: %d" x y colOffset rowOffset
    x + (colOffset * expansionSize), y + (rowOffset * expansionSize)

let gatherGalaxies expansionSize (eu: ExpandedUniverse) =
    let universe = eu.Universe
    [|
        for y = 0 to universe.Length - 1 do
            for x = 0 to universe.[0].Length - 1 do
                if universe.[y].[x] = '#' then
                    yield getOffsetPosition expansionSize eu (x, y)
    |]

let permute arr =
    seq {
        for i = 0 to Array.length arr - 1 do
            for ii = i + 1 to Array.length arr - 1 do
                yield arr.[i], arr.[ii]
    }

let distance (x1, y1) (x2, y2) =
    abs (int64 x1 - int64 x2) + abs (int64 y1 - int64 y2)

readInputFile()

let eu =
    example()
    |> Seq.toArray
    |> expandUniverse

eu

getOffsetPosition 1 eu (0, 4)


eu
|> gatherGalaxies 0
|> permute
|> Seq.toArray
|> Array.map (fun (g1, g2) -> distance g1 g2)
|> Array.sum
