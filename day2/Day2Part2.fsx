// --- Part Two ---
// The Elf says they've stopped producing snow because they aren't getting any water! He isn't sure why the water stopped; however, he can show you how to get to the water source to check it out for yourself. It's just up ahead!

// As you continue your walk, the Elf poses a second question: in each game you played, what is the fewest number of cubes of each color that could have been in the bag to make the game possible?

// Again consider the example games from earlier:

// Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
// Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
// Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
// Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
// Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
// In game 1, the game could have been played with as few as 4 red, 2 green, and 6 blue cubes. If any color had even one fewer cube, the game would have been impossible.
// Game 2 could have been played with a minimum of 1 red, 3 green, and 4 blue cubes.
// Game 3 must have been played with at least 20 red, 13 green, and 6 blue cubes.
// Game 4 required at least 14 red, 3 green, and 15 blue cubes.
// Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag.
// The power of a set of cubes is equal to the numbers of red, green, and blue cubes multiplied together. The power of the minimum set of cubes in game 1 is 48. In games 2-5 it was 12, 1560, 630, and 36, respectively. Adding up these five powers produces the sum 2286.

// For each game, find the minimum set of cubes that must have been present. What is the sum of the power of these sets?

open System.IO
open System.Collections.Generic

type Reveal =
    {
        Red: int
        Green: int
        Blue: int
    }

type Game =
    {
        Id: int
        OriginalString: string
        Reveals: Reveal[]
    }

let splitIntoTwo (line: string, delimiter: string) =
    let index = line.IndexOf(delimiter)
    let left = line.Substring(0, index)
    let right = line.Substring(index + 1)
    left, right

let example =
    [|
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    |]

let parseGame (line: string) =
    let idString, revealsString = splitIntoTwo(line, ":") // "Game 1", "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    let _, id = splitIntoTwo(idString, " ") // "Game", "1"
    let reveals = revealsString.Split(';') // [| "3 blue, 4 red"; "1 red, 2 green, 6 blue"; "2 green" |]
    let revealsWithCubes = 
        reveals
        |> Array.map (fun r ->
            let rgb = Dictionary()
            rgb.Add("red", 0)
            rgb.Add("green", 0)
            rgb.Add("blue", 0)
            let cubes = 
                r.Split(',') // [| "3 blue"; "4 red" |]
                |> Array.map (fun c -> c.Trim().Split(' '))
                |> Array.map (fun cs -> 
                    let count, color = 
                        match cs with
                        | [|count; color|] -> int count, color
                        | _ -> failwithf "Invalid cube: %A" cs

                    rgb[color] <- rgb[color] + count
                )
            { Red = rgb["red"]
              Green = rgb["green"]
              Blue = rgb["blue"] }
        )
    { Id = int id; OriginalString = line; Reveals = revealsWithCubes }

let getMinCubes (game: Game) =
    let maxRed = game.Reveals |> Seq.maxBy (fun r -> r.Red)
    let maxGreen = game.Reveals |> Seq.maxBy (fun r -> r.Green)
    let maxBlue = game.Reveals |> Seq.maxBy (fun r -> r.Blue)
    maxRed.Red, maxGreen.Green, maxBlue.Blue

let getGamePower (game: Game) =
    let r, g, b = getMinCubes(game)
    r * g * b

let readDay2Input () =
    File.ReadAllLines(Path.Combine("day2", "Day2.txt"))

readDay2Input()
|> Array.map parseGame
|> Array.map getGamePower 
|> Array.sum
