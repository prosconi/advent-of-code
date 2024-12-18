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
    |], (7, 7), (6, 6), 1

let splitIntoTwo (delimiter: string) (line: string) =
    match line.Split(delimiter) with
    | [| left; right |] -> left, right
    | _ -> failwithf "Invalid line: %s" line

let readInputFile() =
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Day18.txt")), (71, 71), (70, 70), 1024

type PieceType = Wall | Empty | End

let memoryPositions, size, endPos, startIndex = 
    let data, size, endPos, startIndex = 
        readInputFile()
    data
    |> Seq.map (fun x -> splitIntoTwo "," x)
    |> Seq.map (fun (x, y) -> (int x, int y))
    |> Seq.toArray, size, endPos, startIndex

let width, height = size

let startPos = (0, 0)
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

let inBounds (x, y) = 
    x >= 0 && x < width
        && y >= 0 && y < height

let getPiece (memorySpace: PieceType[,]) (x, y) =
    if (x, y) = endPos
    then End
    else
        if not <| inBounds (x, y)
        then Wall
        else memorySpace[x,y]

let dijsktra n =
    let memorySpace = 
        let truncatedMemoryPositions = 
            memoryPositions
            |> Array.truncate n

        Array2D.init width height (fun x y -> 
            if truncatedMemoryPositions |> Seq.exists (fun (x', y') -> x = x' && y = y')
            then Wall
            else Empty
        )

    let scores = Dictionary()
    scores.Add(startPos, 0)

    let queue = PriorityQueue<_,_>()
    queue.Enqueue(startPos, 0)

    let mutable highScore = None

    while queue.Count > 0 && highScore = None do
        let pos = queue.Dequeue()
        for dirFn in directions do
            let nextPos = dirFn pos
            match getPiece memorySpace nextPos with
            | End -> highScore <- Some(scores[pos] + 1)
            | Empty -> 
                let newScore = scores[pos] + 1
                if not <| scores.ContainsKey nextPos then
                    scores.Add(nextPos, Int32.MaxValue)
                if newScore < scores[nextPos] then
                    scores[nextPos] <- newScore
                    queue.Enqueue(nextPos, newScore)
            | _ -> 
                ()
    highScore

seq { startIndex .. memoryPositions.Length }
|> Seq.tryFind (fun i -> printfn "Trying %d" i; dijsktra i = None)
|> Option.map (fun i -> memoryPositions |> Array.truncate i|> Array.last)
