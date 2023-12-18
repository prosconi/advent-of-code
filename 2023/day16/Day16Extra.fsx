open System

let initialBoard() =
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

type Light =
    {
        mutable X: int
        mutable Y: int
        mutable DX: int
        mutable DY: int
        mutable Color: ConsoleColor
    }
    member this.IsMovingWest() = this.DX < 0
    member this.IsMovingEast() = this.DX > 0
    member this.IsMovingNorth() = this.DY < 0
    member this.IsMovingSouth() = this.DY > 0

let randomConsoleColor() =
    let r = Random()
    let names = Enum.GetNames(typeof<ConsoleColor>)
    let index = r.Next(0, names.Length)
    Enum.Parse(typeof<ConsoleColor>, names.[index]) :?> ConsoleColor

let game() =
    let lights = ResizeArray()
    let lines = initialBoard()
    lights.Add({ X = 0; Y = 0; DX = 1; DY = 0; Color = randomConsoleColor() })

    let energized = lines |> Array.map (fun line -> Array.create (Array.length line) false)
    let mirrorHit = lines |> Array.map (fun line -> Array.create (Array.length line) { WasMovingWest = false; WasMovingEast = false; WasMovingNorth = false; WasMovingSouth = false} )
    let width = 0 |> Array.get lines |> Array.length
    let height = Array.length lines
    
    let moveLight (light: Light) =
        let x, y = light.X, light.Y
        if x < 0 || y < 0 || x >= width || y >= height then lights.Remove(light) |> ignore
        elif light.IsMovingWest() && mirrorHit.[y].[x].WasMovingWest then lights.Remove(light) |> ignore
        elif light.IsMovingEast() && mirrorHit.[y].[x].WasMovingEast then lights.Remove(light) |> ignore
        elif light.IsMovingSouth() && mirrorHit.[y].[x].WasMovingSouth then lights.Remove(light) |> ignore
        elif light.IsMovingNorth() && mirrorHit.[y].[x].WasMovingNorth then lights.Remove(light) |> ignore
        else
            energized[y].[x] <- true

            let c = lines.[y].[x]
            match c with
            | '.' ->
                light.X <- light.X + light.DX
                light.Y <- light.Y + light.DY
            | '/' ->
                if light.IsMovingEast() then
                    mirrorHit[y][x] <- { mirrorHit[y][x] with WasMovingEast = true }
                    light.DX <- 0
                    light.DY <- -1
                    light.Y <- light.Y - 1
                elif light.IsMovingWest() then
                    mirrorHit[y][x] <- { mirrorHit[y][x] with WasMovingWest = true }
                    light.DX <- 0
                    light.DY <- 1
                    light.Y <- light.Y + 1
                elif light.IsMovingNorth() then
                    mirrorHit[y][x] <- { mirrorHit[y][x] with WasMovingNorth = true }
                    light.DX <- 1
                    light.DY <- 0
                    light.X <- light.X + 1
                elif light.IsMovingSouth() then
                    mirrorHit[y][x] <- { mirrorHit[y][x] with WasMovingSouth = true }
                    light.DX <- -1
                    light.DY <- 0
                    light.X <- light.X - 1
            | '\\' -> 
                if light.IsMovingEast() then
                    mirrorHit[y][x] <- { mirrorHit[y][x] with WasMovingEast = true }
                    light.DX <- 0
                    light.DY <- 1
                    light.Y <- light.Y + 1
                elif light.IsMovingWest() then
                    mirrorHit[y][x] <- { mirrorHit[y][x] with WasMovingWest = true }
                    light.DX <- 0
                    light.DY <- -1
                    light.Y <- light.Y - 1
                elif light.IsMovingNorth() then
                    mirrorHit[y][x] <- { mirrorHit[y][x] with WasMovingNorth = true }
                    light.DX <- -1
                    light.DY <- 0
                    light.X <- light.X - 1
                elif light.IsMovingSouth() then
                    mirrorHit[y][x] <- { mirrorHit[y][x] with WasMovingSouth = true }
                    light.DX <- 1
                    light.DY <- 0
                    light.X <- light.X + 1
            | '|' -> 
                if light.IsMovingEast() || light.IsMovingWest() then
                    mirrorHit[y][x] <- { WasMovingEast = true; WasMovingNorth = true; WasMovingSouth = true; WasMovingWest = true }
                    lights.Remove(light) |> ignore
                    lights.Add({ X = x; Y = y + 1; DX = 0; DY = 1; Color = randomConsoleColor() })
                    lights.Add({ X = x; Y = y - 1; DX = 0; DY = -1; Color = randomConsoleColor() })
                else
                    light.X <- light.X + light.DX
                    light.Y <- light.Y + light.DY
            | '-' -> 
                if light.IsMovingNorth() || light.IsMovingSouth() then
                    mirrorHit[y][x] <- { WasMovingEast = true; WasMovingNorth = true; WasMovingSouth = true; WasMovingWest = true }
                    lights.Remove(light) |> ignore
                    lights.Add({ X = x + 1; Y = y; DX = 1; DY = 0; Color = randomConsoleColor() })
                    lights.Add({ X = x - 1; Y = y; DX = -1; DY = 0; Color = randomConsoleColor() })
                else
                    light.X <- light.X + light.DX
                    light.Y <- light.Y + light.DY
            | _ ->
                failwith "invalid input"

    let draw() =
        Console.Clear()
        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                if lines[y][x] = '.' && energized[y][x] then
                    Console.Write('#')
                else
                    Console.Write(lines.[y].[x])
            Console.WriteLine()

        for light in lights |> Seq.toArray do
            if light.X >= 0 && light.Y >= 0 then
                Console.SetCursorPosition(light.X, light.Y)
                Console.ForegroundColor <- light.Color
                Console.Write('X')
                Console.ResetColor()
 
    while lights.Count > 0 do
        System.Threading.Thread.Sleep(200)
        for light in lights |> Seq.toArray do
            moveLight light
            draw()

game()