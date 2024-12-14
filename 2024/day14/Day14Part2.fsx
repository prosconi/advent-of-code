// --- Part Two ---
// During the bathroom break, someone notices that these robots seem awfully similar to ones built and used at the North Pole. If they're the same type of robots, they should have a hard-coded Easter egg: very rarely, most of the robots should arrange themselves into a picture of a Christmas tree.

// What is the fewest number of seconds that must elapse for the robots to display the Easter egg?

open System
open System.IO

let example =
    [|
        "p=0,4 v=3,-3"
        "p=6,3 v=-1,-3"
        "p=10,3 v=-1,2"
        "p=2,0 v=2,-1"
        "p=0,0 v=1,3"
        "p=3,0 v=-2,-2"
        "p=7,6 v=-1,-3"
        "p=3,0 v=-1,-2"
        "p=9,3 v=2,3"
        "p=7,3 v=-1,2"
        "p=2,4 v=2,-3"
        "p=9,5 v=-3,-3"
    |]

let splitIntoTwo (ch: char) (str: string) = 
    match str.Split ch with
    | [| partA; partB |] -> (partA, partB)
    | _ -> failwithf "Error in code: %s, ch=%A" str ch

let parseCoords (str: string) =
    let (_, v) = splitIntoTwo '=' str
    let (x, y) = splitIntoTwo ',' v
    (int x, int y)

type Robot =
    {
        mutable Position: (int * int)
        Velocity: (int * int)
    }

let parseLine (str: string) =
    let (a, b) = splitIntoTwo ' ' str
    let pos = parseCoords a
    let vel = parseCoords b
    { Position = pos; Velocity = vel }


let exampleData() = 
    let robots = 
        example
        |> Array.map parseLine
    11, 7, robots

let fileData() =
    let robots = 
        Path.Combine(__SOURCE_DIRECTORY__, "Day14.txt")
        |> File.ReadAllLines
        |> Array.map parseLine

    101, 103, robots

let tickRobot (width, height) (r: Robot) =
    let dx, dy = r.Velocity
    let x, y = r.Position

    let newX = 
        let x' = x + dx
        if x' < 0 then width + x'
        else if x' >= width then x' - width
        else x'

    let newY = 
        let y' = y + dy
        if y' < 0 then height + y'
        else if y' >= height then y' - height
        else y'

    r.Position <- newX, newY

let tickRobots dimensions (robots: Robot array) =
    robots
    |> Array.iter (tickRobot dimensions)

let isSafe (width, height) (r: Robot) =
    let middleX, middleY = width / 2, height / 2
    let x, y = r.Position
    
    if x = middleX || y = middleY
    then false
    else true
    
let toValues dimensions data =
    data
    |> Array.map _.Position
    |> Array.groupBy id
    |> Array.map (fun (k, v) -> (k, Array.length v))

let printRobots (width, height) data =
    Console.Clear()

    let values = toValues (width, height) data

    for y = 0 to height - 1 do
        for x = 0 to width - 1 do
            let v = values
                    |> Array.tryFind (fun (k, _) -> k = (x, y))
                    |> Option.map snd
            match v with
            | Some v -> Console.Write(v)
            | None -> Console.Write('.')
        Console.WriteLine()

let simulate() =
    let width, height, d = fileData()
    let mutable frameNumber = 0
    while true do
        frameNumber <- frameNumber + 1
        tickRobots (width, height) d

        let values = toValues (width, height) d
        match values |> Array.forall (fun (_, v) -> v = 1) with
        | true ->
            printRobots (width, height) d
            Console.WriteLine("Frame: {0}", frameNumber)
            System.Threading.Thread.Sleep(1_000)
        | false -> 
            Console.WriteLine("Frame: {0}", frameNumber)

simulate()