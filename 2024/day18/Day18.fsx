open System
open System.IO
open System.Collections.Generic

let example() =
    [|
        "5,4"
        "4,2"
        "4,5"
        "3,0"
        "2,1"
        "6,3"
        "2,4"
        "1,5"
        "0,6"
        "3,3"
        "2,6"
        "5,1"
        "1,2"
        "5,5"
        "2,5"
        "6,5"
        "1,4"
        "0,4"
        "6,4"
        "1,1"
        "6,1"
        "1,0"
        "0,5"
        "1,6"
        "2,0"
    |]
    |> Array.truncate 12, (7, 7), (6, 6)

let splitIntoTwo (delimiter: string) (line: string) =
    match line.Split(delimiter) with
    | [| left; right |] -> left, right
    | _ -> failwithf "Invalid line: %s" line

let readInputFile() =
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Day18.txt"))
    |> Array.truncate 1024, (71, 71), (70, 70)

type PieceType = Wall | Empty | End

let memoryPositions, size, endPos = 
    let data, size, endPos = 
        readInputFile()
    data
    |> Seq.map (fun x -> splitIntoTwo "," x)
    |> Seq.map (fun (x, y) -> (int x, int y))
    |> Seq.toArray, size, endPos

let width, height = size

let memorySpace = 
    Array2D.init width height (fun x y -> 
        if memoryPositions |> Seq.exists (fun (x', y') -> x = x' && y = y')
        then Wall
        else Empty
    )

let startPos = (0, 0)
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

let inBounds (x, y) = 
    x >= 0 && x < width
        && y >= 0 && y < height

let getPiece (x, y) =
    if (x, y) = endPos
    then End
    else
        if not <| inBounds (x, y)
        then Wall
        else memorySpace[x,y]

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
                | End -> Console.Write 'E'
                | Empty -> Console.Write '.'
        Console.WriteLine()
    Threading.Thread.Sleep(10)

let dijsktra() =
    let scores = Dictionary()
    scores.Add({ Pos = startPos; Dir = East }, 0)

    let queue = PriorityQueue<_,_>()
    queue.Enqueue({ Pos = startPos; Dir = East }, 0)

    while queue.Count > 0 do
        // printMaze scores
        let d = queue.Dequeue()
        for direction, fn in directions do
            let nextPos = fn d.Pos
            match getPiece nextPos with
            | End -> failwithf "Done! %A" <| scores[d] + 1
            | Empty -> 
                let newScore = scores[d] + 1
                let nextD = { Pos = nextPos; Dir = direction }
                if not <| scores.ContainsKey nextD then
                    scores.Add(nextD, Int32.MaxValue)
                if newScore < scores[nextD] then
                    scores[nextD] <- newScore
                    queue.Enqueue(nextD, newScore)
            | _ -> 
                ()
    scores

dijsktra()