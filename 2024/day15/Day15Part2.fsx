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

type Piece = Wall | Ship | LeftBox | RightBox | Empty
type Direction = Up | Down | Left | Right
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

let charToPiece (ch: char) =
    match ch with
    | '#' -> [Wall; Wall]
    | '@' -> [Ship; Empty]
    | 'O' -> [LeftBox; RightBox]
    | '.' -> [Empty; Empty]
    | _ -> failwithf "Invalid piece: %A" ch

let charToDirection (ch: char) =
    match ch with
    | '^' -> Up
    | 'v' -> Down
    | '<' -> Left
    | '>' -> Right
    | _ -> failwithf "Invalid direction: %A" ch

let nextCoords (x, y) (d: Direction) =
    match d with
    | Up -> (x, y - 1)
    | Down -> (x, y + 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

let fileLines = 
    Path.Combine(__SOURCE_DIRECTORY__, "Day15.txt") |> File.ReadLines

let p, d = 
    example
    |> Seq.filter (fun x -> x <> "")
    |> Seq.map (fun x -> 
        if x.StartsWith "#"
        then Pieces (x |> Seq.collect charToPiece |> Seq.toArray)
        else Directions (x |> Seq.map charToDirection |> Seq.toArray)
    )
    |> Seq.toArray
    |> Array.partition (fun x -> 
        match x with
        | Pieces _ -> true
        | _ -> false
    )

let pieces = p |> Array.map _.GetPieces
let directions = d |> Array.collect _.GetDirections
let mutable shipLocation = 
    pieces
    |> Array.mapi (fun row x -> 
        match x |> Array.tryFindIndex (fun y -> y = Ship) with
        | Some col -> Some (col, row)
        | None -> None
    )
    |> Array.choose id
    |> Seq.head

let printPieces() =
    for y = 0 to pieces.Length - 1 do
        for x = 0 to pieces[y].Length - 1 do
            match pieces[y][x] with
            | Wall -> Console.Write '#'
            | Ship -> Console.Write '@'
            | LeftBox -> Console.Write '['
            | RightBox -> Console.Write ']'
            | Empty -> Console.Write '.'
        Console.WriteLine()

let getPiece (x, y) = pieces[y][x]

let pushBox (x, y) direction =
    let mutable exit = false
    let mutable nextX, nextY = nextCoords (x, y) direction
    let startX = nextX
    let startY = nextY
    let mutable canMove = false
    while not(exit) do
        let piece = getPiece (nextX, nextY)
        match piece with
        | Wall -> 
            exit <- true
        | LeftBox | RightBox -> 
            let x, y = nextCoords (nextX, nextY) direction
            nextX <- x
            nextY <- y
        | Empty -> 
            exit <- true
            canMove <- true
        | Ship -> 
            failwith "Ship is not where it should be"
    
    if canMove then
        pieces[y][x] <- Empty
        match direction with
        | Up -> 
            for y in [nextY .. -1 .. startY] do 
                pieces[y][x] <- RightBox
                pieces[y][x - 1] <- LeftBox
        | Down -> 
            for y in [startY .. 1 .. nextY] do 
                pieces[y][x] <- RightBox
                pieces[y][x - 1] <- LeftBox
        | Left -> 
            printfn "Left: %d, %d" nextX startX
            for x in [nextX .. -2 .. startX] do
                pieces[y][x] <- RightBox
                pieces[y][x - 1] <- LeftBox
        | Right -> 
            printfn "Right: %d, %d" nextX startX
            for x in [startX .. 2 .. nextX] do 
                pieces[y][x] <- LeftBox
                pieces[y][x + 1] <- RightBox
    canMove

let moveShip direction =
    let currentX, currentY = shipLocation
    let nextX, nextY = nextCoords shipLocation direction
    let piece = getPiece (nextX, nextY)
    match piece with
    | Wall -> ()
    | LeftBox | RightBox ->
        if pushBox (nextX, nextY) direction then
            pieces[currentY][currentX] <- Empty
            pieces[nextY][nextX] <- Ship
            shipLocation <- (nextX, nextY)
    | Empty ->
        pieces[currentY][currentX] <- Empty
        pieces[nextY][nextX] <- Ship
        shipLocation <- (nextX, nextY)
    | Ship -> 
        failwithf "Ship is not where it should be: %A" shipLocation

moveShip Left
printPieces()
shipLocation

for direction in directions do
    moveShip direction

pieces
|> Array.mapi (fun i x ->
    x
    |> Array.mapi (fun ii xx ->
        match xx with
        | LeftBox -> Some(i * 100 + ii)
        | _ -> None
    )
)
|> Array.collect id
|> Array.choose id
|> Array.sum

