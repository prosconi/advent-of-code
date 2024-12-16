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

type Rect =
    {
        Left: int
        Top: int
        Width: int
        Height: int
    }
    member this.Right = this.Left + this.Width
    member this.Bottom = this.Top + this.Height

type Piece(pieceType, position) =
    let mutable position = position

    member this.PieceType = pieceType

    member this.Postion = position

    member this.SetPosition(newPosition) =
        position <- newPosition

    member this.CanPush(direction, pieces: seq<Piece>) =
        let x, y = this.Postion
        let nextPosition = nextCoords (x, y) direction

        let otherPieces = 
            pieces
            |> Seq.filter (fun x -> x <> this)
            |> Seq.filter (fun x -> x.Intersects nextPosition this.Width)
            |> Seq.toArray

        match otherPieces with
        | [||] -> true
        | others -> 
            others
            |> Seq.forall (fun other -> 
                match other.PieceType with
                | Box -> other.CanPush(direction, pieces)
                | _ -> false
            )

    member this.TryPush(direction, pieces: seq<Piece>) =
        let x, y = this.Postion
        let nextPosition = nextCoords (x, y) direction

        let otherPieces = 
            pieces
            |> Seq.filter (fun x -> x <> this)
            |> Seq.filter (fun x -> x.Intersects nextPosition this.Width)
            |> Seq.toArray

        match otherPieces with
        | [||] -> 
            this.SetPosition(nextPosition)
        | others -> 
            others
            |> Seq.iter (fun other -> 
                match other.PieceType with
                | Box -> 
                    other.TryPush(direction, pieces)
                    this.SetPosition(nextPosition)
                | _ -> ()
            )

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
        let r1 = { Left = this.Left; Top = this.Top; Width = this.Width; Height = 1 }
        let r2 = { Left = otherX; Top = otherY; Width = width; Height = 1 }
        r1.Left <= r2.Right
            && r1.Right >= r2.Left
            && r1.Top = r2.Top

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
    Path.Combine(__SOURCE_DIRECTORY__, "Day15.txt")
    |> File.ReadLines
    |> Seq.toArray

let pieces = ResizeArray()
let directions = ResizeArray()
let data = fileLines

for y = 0 to data.Length - 1 do
    let line = data[y]
    if line = "" then 
        ()
    elif line.StartsWith "#" then 
        for x = 0 to data[y].Length - 1 do
            let ch = data[y][x]
            let realX = x * 2
            let position = (realX, y)
            charToPiece ch position |> Option.iter(pieces.Add)
    else
        for ch in line do
            directions.Add(charToDirection ch)

let ship = pieces |> Seq.find(fun p -> p.PieceType = Ship)

let drawPieces() =
    Console.Clear()
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
            if p.CanPush(direction, pieces) then
                p.TryPush(direction, pieces)
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

let simulationLoop() =
    for d in directions do
        tryMove d

    pieces
    |> Seq.map (fun p ->
        let x, y = p.Postion
        match p.PieceType with
        | Box -> Some(x + y * 100)
        | _ -> None
    )
    |> Seq.choose id
    |> Seq.sum

simulationLoop()
//gameLoop()
