open System
open System.IO
open System.Collections.Generic

let example =
    [|
        "###############"
        "#...#...#.....#"
        "#.#.#.#.#.###.#"
        "#S#...#.#.#...#"
        "#######.#.#.###"
        "#######.#.#...#"
        "#######.#.###.#"
        "###..E#...#...#"
        "###.#######.###"
        "#...###...#...#"
        "#.#####.#.###.#"
        "#.#...#.#.#...#"
        "#.#.#.#.#.#.###"
        "#...#...#...###"
        "###############"
    |]

let readInputFile() =
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Day20.txt"))

type PieceType = Wall | Empty | Start | End

type Maze = 
    {
        Data: PieceType[][]
        Width: int
        Height: int
        StartPos: (int * int)
        EndPos: (int * int)
    }

let lines = readInputFile()

let getMaze (removeX, removeY) = 
    let mutable startPos = (0,0)
    let mutable endPos = (0,0)
    let data = 
        lines
        |> Array.mapi (fun y row -> 
            row
            |> Seq.mapi (fun x ch ->
                let ch = 
                    if x = removeX && y = removeY
                    then '.'
                    else ch
                match ch with
                | '#' -> Wall
                | 'S' -> startPos <- (x, y); Start
                | 'E' -> endPos <- (x, y); End
                | '.' -> Empty
                | _ -> failwithf "Invalid piece: %A" ch
            )
            |> Seq.toArray
        )

    { Data = data
      Width = data.[0].Length
      Height = data.Length
      StartPos = startPos
      EndPos = endPos }

let inBounds (m: Maze) (x, y) = 
    x >= 0 && x < m.Width
        && y >= 0 && y < m.Height

let getPiece (m: Maze) (x, y) =
    if not <| inBounds m (x, y)
    then Wall
    else m.Data[y][x]

let north (x, y) = (x, y - 1)
let south (x, y) = (x, y + 1)
let east (x, y) = (x + 1, y)
let west (x, y) = (x - 1, y)
let directions = 
    [
        north
        south
        east
        west
    ]

type D = int * int
type D2 = { Pos: (int * int); Path: ResizeArray<int * int> }

let printMaze (m: Maze) (scores: Dictionary<D,int>) =
    let scores = scores |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> dict
    Console.Clear()
    for y = 0 to m.Height - 1 do
        for x = 0 to m.Width - 1 do
            if scores.ContainsKey(x, y)
            then
                Console.ForegroundColor <- ConsoleColor.Green 
                Console.Write 'X'
                Console.ResetColor()
            else
                match getPiece m (x, y) with
                | Wall -> Console.Write '#'
                | Start -> Console.Write 'S'
                | End -> Console.Write 'E'
                | Empty -> Console.Write ' '
        Console.WriteLine()
    Threading.Thread.Sleep(10)

let printMazeWithPath (m: Maze) (path: seq<int*int>) =
    let dict = path |> Seq.map (fun pos -> pos, 0) |> dict
    Console.Clear()
    for y = 0 to m.Height - 1 do
        for x = 0 to m.Width - 1 do
            if dict.ContainsKey(x, y)
            then
                Console.ForegroundColor <- ConsoleColor.Green 
                Console.Write 'X'
                Console.ResetColor()
            else
                match getPiece m (x, y) with
                | Wall -> Console.Write '#'
                | Start -> Console.Write 'S'
                | End -> Console.Write 'E'
                | Empty -> Console.Write ' '
        Console.WriteLine()
    Threading.Thread.Sleep(10)

let dijsktra(m: Maze) =
    let scores = Dictionary()
    let initialPosition = m.StartPos
    scores.Add(initialPosition, 0)

    let queue = PriorityQueue<_,_>()
    queue.Enqueue({ Pos = m.StartPos; Path = ResizeArray([m.StartPos]) }, 0)

    let results = ResizeArray()

    while queue.Count > 0 do
        let d = queue.Dequeue()
        let sd = d.Pos
        for fn in directions do
            let nextPos = fn d.Pos
            match getPiece m nextPos with
            | End -> 
                results.Add(scores[sd] + 1, d.Path)
            | Empty -> 
                let additionalCost = 1
                let newScore = scores[sd] + additionalCost
                let nextD = nextPos
                if not <| scores.ContainsKey nextD then
                    scores.Add(nextD, Int32.MaxValue)
                if newScore <= scores[nextD] then
                    scores[nextD] <- newScore
                    let arr = ResizeArray(d.Path)
                    arr.Add(nextPos)
                    queue.Enqueue({ Pos = nextPos; Path = arr }, newScore)
            | _ -> 
                ()
    results

let allData =
    let firstMaze = getMaze (-1, -1)
    let firstSolve = dijsktra firstMaze
    let min = firstSolve |> Seq.map fst |> Seq.min
    seq {
        for x = 0 to firstMaze.Width do
            for y = 0 to firstMaze.Height do
                printfn "Solving: %d, %d" x y
                let m = getMaze(x, y)
                dijsktra m
    }
    |> Seq.collect (fun x -> x)
    |> Seq.map (fun (a, b) -> min - a)
    |> Seq.countBy id
    |> Seq.toArray

allData
|> Seq.filter (fun (a, b) -> a >= 100)
|> Seq.sumBy snd
