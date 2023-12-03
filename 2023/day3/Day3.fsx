// --- Day 3: Gear Ratios ---
// You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up to the water source, but this is as far as he can bring you. You go inside.

// It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.

// "Aaah!"

// You turn around to see a slightly-greasy Elf with a wrench and a look of surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I can fix it." You offer to help.

// The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one. If you can add up all the part numbers in the engine schematic, it should be easy to work out which part is missing.

// The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)

// Here is an example engine schematic:

// 467..114..
// ...*......
// ..35..633.
// ......#...
// 617*......
// .....+.58.
// ..592.....
// ......755.
// ...$.*....
// .664.598..
// In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is 4361.

// Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?

open System
open System.IO

let exampleSchematic = 
    [|
        "467..114.."
        "...*......"
        "..35..633."
        "......#..."
        "617*......"
        ".....+.58."
        "..592....."
        "......755."
        "...$.*...."
        ".664.598.."
    |]

let testSchematic = 
    [|
        ".114"
        "...*"
    |]

let readInputFile() = 
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Day3.txt"))

let isSymbol c =
    if Char.IsDigit c then false
    elif c = '.' then false
    else true

// isSymbol '1' // false
// isSymbol '$' // true
// isSymbol '.' // false

let findEndIndex line index =
    let length = String.length line
    seq { index .. length }
    |> Seq.tryFind (fun i -> i = length || not <| Char.IsDigit line[i])
    |> Option.map (fun x -> x - 1)
    |> Option.defaultValue index

// findEndIndex "4" 0 // 0
// findEndIndex "42" 0 // 1
// findEndIndex "42." 0 // 1
// findEndIndex "617*......" 0 // 2
// findEndIndex "617" 0 // 2

let check schematic newX newY =
    if newX >= 0 && newY >= 0 then
        if newY < Array.length schematic then
            let line = schematic[newY]
            if newX < String.length line then
                let c = line[newX]
                isSymbol c
            else
                false
        else
            false
    else
        false

// check [| "467..114.." |] 0 0 // false
// check [| "467*.114.." |] 3 0 // true

let checkPosition (schematic: string[]) x y =
    let LEFT = -1
    let RIGHT = 1
    let UP = -1
    let DOWN = 1
    let STAY = 0
    let checks =
        seq {
            check schematic (x + LEFT)  (y + STAY) // left
            check schematic (x + RIGHT) (y + STAY) // right
            check schematic (x + STAY)  (y + UP)   // up
            check schematic (x + STAY)  (y + DOWN) // down
            check schematic (x + LEFT)  (y + UP)   // up left
            check schematic (x + LEFT)  (y + DOWN) // down left
            check schematic (x + RIGHT) (y + UP)   // up right
            check schematic (x + RIGHT) (y + DOWN) // down right
        }

    checks
    |> Seq.exists id

// checkPosition [| "467*114.." |] 0 0 // false
// checkPosition [| "467*114.." |] 0 0 // false
// checkPosition [| "467*114.." |] 1 0 // false
// checkPosition [| "467*114.." |] 2 0 // true
// checkPosition [| "467*114.." |] 3 0 // false
// checkPosition [| "467*114.." |] 4 0 // true
// checkPosition [| "467*114.." |] 5 0 // false

// checkPosition [| "467*114.."; "*1.2....." |] 0 0 // true
// checkPosition [| "467*114.."; "*1.2....." |] 0 1 // false
// checkPosition [| "467*114.."; "*1.2....." |] 1 1 // true
// checkPosition [| "467*114.."; "*1.2....." |] 2 1 // true
// checkPosition [| "467*114.."; "*1.2....." |] 3 1 // true
// checkPosition [| "467*114.."; "*1.2....." |] 4 1 // true
// checkPosition [| "467*114.."; "*1.2....." |] 5 1 // false

let getNumber (line: string) startIndex endIndex =
    int (line[startIndex..endIndex])

let parseSchematic schematic =
    seq {
        let mutable y = 0
        while y < Array.length schematic do
            let line = schematic[y]
            let mutable x = 0
            while x < String.length line do
                let c = line[x]
                if Char.IsDigit c then
                    let endIndex = findEndIndex line x
                    let startIndex = x
                    while x <= endIndex do
                        let res = checkPosition schematic x y
                        match res with
                        | false -> ()
                        | true -> 
                            let n = getNumber line startIndex endIndex
                            yield n
                            x <- endIndex
                        x <- x + 1
                x <- x + 1
            y <- y + 1
    }

readInputFile()
|> parseSchematic
|> Seq.sum
