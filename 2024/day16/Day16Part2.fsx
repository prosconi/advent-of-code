open System
open System.IO
open System.Collections.Generic

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

type Facing =
    | North
    | South
    | East
    | West

let directions = 
    [
        North, north
        South, south
        East, east
        West, west
    ]

type D = { Pos: (int * int); Dir: Facing }
type D2 = { Pos: (int * int); Dir: Facing; Path: ResizeArray<int * int> }

let printMaze (scores: Dictionary<D,int>) =
    let scores = scores |> Seq.map (fun kvp -> kvp.Key.Pos, kvp.Value) |> dict
    Console.Clear()
    for y = 0 to height - 1 do
        for x = 0 to width - 1 do
            if scores.ContainsKey(x, y)
            then
                Console.ForegroundColor <- ConsoleColor.Green 
                Console.Write 'X'
                Console.ResetColor()
            else
                match getPiece (x, y) with
                | Wall -> Console.Write '#'
                | Start -> Console.Write 'S'
                | End -> Console.Write 'E'
                | Empty -> Console.Write ' '
        Console.WriteLine()
    Threading.Thread.Sleep(10)

let printMazeWithPath (path: seq<int*int>) =
    let dict = path |> Seq.map (fun pos -> pos, 0) |> dict
    Console.Clear()
    for y = 0 to height - 1 do
        for x = 0 to width - 1 do
            if dict.ContainsKey(x, y)
            then
                Console.ForegroundColor <- ConsoleColor.Green 
                Console.Write 'X'
                Console.ResetColor()
            else
                match getPiece (x, y) with
                | Wall -> Console.Write '#'
                | Start -> Console.Write 'S'
                | End -> Console.Write 'E'
                | Empty -> Console.Write ' '
        Console.WriteLine()
    Threading.Thread.Sleep(10)

let dijsktra() =
    let scores = Dictionary()
    let initialPosition = { Pos = startPos; Dir = East }
    scores.Add(initialPosition, 0)

    let queue = PriorityQueue<_,_>()
    queue.Enqueue({ Pos = startPos; Dir = East; Path = ResizeArray([startPos]) }, 0)

    let results = ResizeArray()

    while queue.Count > 0 do
        let d = queue.Dequeue()
        let sd = { Pos = d.Pos; Dir = d.Dir }
        for direction, fn in directions do
            let nextPos = fn d.Pos
            match getPiece nextPos with
            | End -> 
                //scores.Add({ Pos = nextPos; Dir = direction }, scores[sd] + 1)
                results.Add(scores[sd] + 1, d.Path)
            | Empty -> 
                let additionalCost = if direction = d.Dir then 1 else 1001

                let newScore = scores[sd] + additionalCost
                let nextD = { Pos = nextPos; Dir = direction }
                if not <| scores.ContainsKey nextD then
                    scores.Add(nextD, Int32.MaxValue)
                if newScore <= scores[nextD] then
                    scores[nextD] <- newScore
                    let arr = ResizeArray(d.Path)
                    arr.Add(nextPos)
                    queue.Enqueue({ Pos = nextPos; Dir = direction; Path = arr }, newScore)
            | _ -> 
                ()
    results

let results = dijsktra()
let min = results |> Seq.map (fun (a, b) -> a) |> Seq.min
let allMins = results |> Seq.filter (fun (a, b) -> a = min) |> Seq.toArray
let allPaths = 
    allMins
    |> Seq.collect (fun (a, b) -> b)
    |> Seq.distinct
    |> Seq.toArray

printMazeWithPath allPaths
printfn "Length: %d" (allPaths.Length + 1)
