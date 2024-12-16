open System
open System.IO

let example =
    [|
        "##########"
        "#..O..O.O#"
        "#......O.#"
        "#.OO..O.O#"
        "#..O@..O.#"
        "#O#..O...#"
        "#O..O..O.#"
        "#.OO.O.OO#"
        "#....O...#"
        "##########"
        ""
        "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^"
        "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v"
        "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<"
        "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^"
        "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><"
        "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^"
        ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^"
        "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>"
        "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>"
        "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
    |]

type PieceType = Wall | Ship | Box
type Direction = Up | Down | Left | Right

let nextCoords (x, y) (d: Direction) =
    match d with
    | Up -> (x, y - 1)
    | Down -> (x, y + 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

type Piece(pieceType, position) =
    let mutable position = position

    member this.PieceType = pieceType

    member this.Postion = position

    member this.SetPosition(newPosition) =
        position <- newPosition

    member this.TryPush(direction, pieces: seq<Piece>) =
        let x, y = this.Postion
        let checkPosition, nextPosition = 
            match direction with
            | Right -> (x + 2, y), (x + 1, y)
            | Left  -> (x - 1, y), (x - 1, y)
            | Up    -> (x, y - 1), (x, y - 1)
            | Down  -> (x, y + 1), (x, y + 1)

        match pieces |> Seq.tryFind (fun x -> x.Intersects checkPosition this.Width) with
        | Some other -> 
            match other.PieceType with
            | Box -> 
                if other.TryPush(direction, pieces) then
                    this.SetPosition(nextPosition)
                    true
                else
                    false
            | _ -> false
        | None -> 
            this.SetPosition(nextPosition)
            true

    member this.Render() =
        Console.SetCursorPosition(this.Postion)
        match pieceType with
        | Wall -> Console.Write "##"
        | Ship -> Console.Write "@"
        | Box -> Console.Write "[]"

    member this.Width = 
        match pieceType with
        | Wall -> 1
        | Ship -> 0
        | Box -> 1

    member this.Left = fst(this.Postion)

    member this.Right = this.Left + this.Width

    member this.Top = snd(this.Postion)

    member this.Bottom = this.Top

    member this.Intersects (otherX, otherY) width =
        let x, y = position
        match pieceType with
        | Ship -> false
        | Wall
        | Box -> 
            x = otherX && y = otherY ||
                x = otherX + width && y = otherY

type Line = 
    | Pieces of Piece[]
    | Directions of Direction[]
    member this.GetPieces = 
        match this with
        | Pieces p -> p
        | _ -> failwith "Not pieces"
    member this.GetDirections = 
        match this with
        | Directions p -> p
        | _ -> failwith "Not directions"

let charToPiece (ch: char) position =
    match ch with
    | '#' -> Some(Piece(Wall, position))
    | '@' -> Some(Piece(Ship, position))
    | 'O' -> Some(Piece(Box, position))
    | _   -> None

let charToDirection (ch: char) =
    match ch with
    | '^' -> Up
    | 'v' -> Down
    | '<' -> Left
    | '>' -> Right
    | _ -> failwithf "Invalid direction: %A" ch

let fileLines = 
    Path.Combine(__SOURCE_DIRECTORY__, "Day15.txt") |> File.ReadLines

let pieces = ResizeArray()
let directions = ResizeArray()
for y = 0 to example.Length - 1 do
    let line = example[y]
    if line = "" then 
        ()
    elif line.StartsWith "#" then 
        for x = 0 to example[y].Length - 1 do
            let ch = example[y][x]
            let realX = x * 2
            let position = (realX, y)
            charToPiece ch position |> Option.iter(pieces.Add)
    else
        for ch in line do
            directions.Add(charToDirection ch)

let ship = pieces |> Seq.find(fun p -> p.PieceType = Ship)

let drawPieces() =
    for piece in pieces do
        piece.Render()

    Console.SetCursorPosition(20, 10)
    Console.Write(sprintf "%A" ship.Postion)
    
let tryMove direction =
    let next = nextCoords ship.Postion direction
    match pieces |> Seq.tryFind (fun x -> x.Intersects next 0) with
    | Some p -> 
        match p.PieceType with
        | Box -> 
            if p.TryPush(direction, pieces) then
                ship.SetPosition(next)
        | _ -> ()
    | None -> 
        ship.SetPosition(next)

let rec gameLoop() =
    Console.Clear()
    drawPieces()
    
    let key = Console.ReadKey()
    match key.Key with
    | ConsoleKey.LeftArrow -> 
        tryMove Left
        gameLoop()
    | ConsoleKey.RightArrow -> 
        tryMove Right
        gameLoop()
    | ConsoleKey.UpArrow -> 
        tryMove Up
        gameLoop()
    | ConsoleKey.DownArrow -> 
        tryMove Down
        gameLoop()
    | ConsoleKey.Escape -> 
        ()
    | _ -> gameLoop()

gameLoop()
