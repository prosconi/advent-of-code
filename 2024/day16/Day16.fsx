open System
open System.IO

let example2 = 
    [|
        "#################"
        "#...#...#...#..E#"
        "#.#.#.#.#.#.#.#.#"
        "#.#.#.#...#...#.#"
        "#.#.#.#.###.#.#.#"
        "#...#.#.#.....#.#"
        "#.#.#.#.#.#####.#"
        "#.#...#.#.#.....#"
        "#.#.#####.#.###.#"
        "#.#.#.......#...#"
        "#.#.###.#####.###"
        "#.#.#...#.....#.#"
        "#.#.#.#####.###.#"
        "#.#.#.........#.#"
        "#.#.#.#########.#"
        "#S#.............#"
        "#################"

    |]

let example =
    [|
        "###############"
        "#.......#....E#"
        "#.#.###.#.###.#"
        "#.....#.#...#.#"
        "#.###.#####.#.#"
        "#.#.#.......#.#"
        "#.#.#####.###.#"
        "#...........#.#"
        "###.#.#####.#.#"
        "#...#.....#.#.#"
        "#.#.#.###.#.#.#"
        "#.....#...#.#.#"
        "#.###.#.#.#.#.#"
        "#S..#.....#...#"
        "###############"
    |]


let fileLines = 
    Path.Combine(__SOURCE_DIRECTORY__, "Day16.txt")
    |> File.ReadLines
    |> Seq.toArray

type PieceType = Wall | Empty | Start | End

let mutable startPos = (0,0)
let mutable endPos = (0,0)

let data = 
    fileLines
    |> Array.mapi (fun y row -> 
        row
        |> Seq.mapi (fun x ch ->
            match ch with
            | '#' -> Wall
            | 'S' -> startPos <- (x, y); Start
            | 'E' -> endPos <- (x, y); End
            | '.' -> Empty
            | _ -> failwithf "Invalid piece: %A" ch
        )
        |> Seq.toArray
    )

let width = data.Length
let height = data[0].Length

let inBounds (x, y) = 
    x >= 0 && x < width
        && y >= 0 && y < height

let getPiece (x, y) =
    if not <| inBounds (x, y)
    then Wall
    else data[y][x]

let north (x, y) = (x, y - 1)
let south (x, y) = (x, y + 1)
let east (x, y) = (x + 1, y)
let west (x, y) = (x - 1, y)

let printMaze path = 
    for y = 0 to height - 1 do
        for x = 0 to width - 1 do
            if Set.contains (x, y) path
            then Console.Write 'X'
            else
                match getPiece (x, y) with
                | Wall -> Console.Write '#'
                | Start -> Console.Write 'S'
                | End -> Console.Write 'E'
                | Empty -> Console.Write '.'
        Console.WriteLine()
    System.Threading.Thread.Sleep 100

type Facing =
    | North
    | South
    | East
    | West

let rec go path turnCount facing pos =
    // printMaze path
    let directions = 
        [
            North, north
            South, south
            East, east
            West, west
        ]
    seq {
        for direction, d in directions do
            let turning = 
                if direction <> facing
                then 1
                else 0

            let newPos = d pos
            if Set.contains newPos path
            then 0, 0
            else 
                match getPiece newPos with
                | Wall -> 
                    0, 0
                | Empty ->
                    yield! go (Set.add pos path) (turnCount + turning) direction newPos
                | Start ->
                    0, 0
                | End ->
                    printfn "Found end!"
                    turnCount, (Set.count path) + 1
    }

go Set.empty 0 East startPos
|> Seq.filter (fun (turns, _) -> turns > 0)
|> Seq.map (fun (turnCount, stepCount) -> turnCount * 1000 + stepCount)
|> Seq.min
