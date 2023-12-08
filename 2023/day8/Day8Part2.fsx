// --- Part Two ---
// The sandstorm is upon you and you aren't any closer to escaping the wasteland. You had the camel follow the instructions, but you've barely left your starting position. It's going to take significantly more steps to escape!

// What if the map isn't for people - what if the map is for ghosts? Are ghosts even bound by the laws of spacetime? Only one way to find out.

// After examining the maps a bit longer, your attention is drawn to a curious fact: the number of nodes with names ending in A is equal to the number ending in Z! If you were a ghost, you'd probably just start at every node that ends with A and follow all of the paths at the same time until they all simultaneously end up at nodes that end with Z.

// For example:

// LR

// 11A = (11B, XXX)
// 11B = (XXX, 11Z)
// 11Z = (11B, XXX)
// 22A = (22B, XXX)
// 22B = (22C, 22C)
// 22C = (22Z, 22Z)
// 22Z = (22B, 22B)
// XXX = (XXX, XXX)
// Here, there are two starting nodes, 11A and 22A (because they both end with A). As you follow each left/right instruction, use that instruction to simultaneously navigate away from both nodes you're currently on. Repeat this process until all of the nodes you're currently on end with Z. (If only some of the nodes you're on end with Z, they act like any other node and you continue as normal.) In this example, you would proceed as follows:

// Step 0: You are at 11A and 22A.
// Step 1: You choose all of the left paths, leading you to 11B and 22B.
// Step 2: You choose all of the right paths, leading you to 11Z and 22C.
// Step 3: You choose all of the left paths, leading you to 11B and 22Z.
// Step 4: You choose all of the right paths, leading you to 11Z and 22B.
// Step 5: You choose all of the left paths, leading you to 11B and 22C.
// Step 6: You choose all of the right paths, leading you to 11Z and 22Z.
// So, in this example, you end up entirely on nodes that end in Z after 6 steps.

// Simultaneously start on every node that ends with A. How many steps does it take before you're only on nodes that end with Z?


open System.IO
open System.Collections.Generic

let example() =
    [|
        "LR"
        ""
        "11A = (11B, XXX)"
        "11B = (XXX, 11Z)"
        "11Z = (11B, XXX)"
        "22A = (22B, XXX)"
        "22B = (22C, 22C)"
        "22C = (22Z, 22Z)"
        "22Z = (22B, 22B)"
        "XXX = (XXX, XXX)"
    |]

let readInputFile() =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Day8.txt"))

let splitIntoTwo (delimiter: string) (line: string) =
    let index = line.IndexOf(delimiter)
    let left = line.Substring(0, index)
    let right = line.Substring(index + delimiter.Length)
    left, right

type Map = { directions: string; maps: IDictionary<string, string * string> }

let isStartingPosition (str: string) = str.EndsWith "A"
let isEndingPosition (str: string) = str.EndsWith "Z"

let parse lines =
    let header = lines |> Seq.head
    let maps = 
        lines
        |> Seq.skip 2
        |> Seq.map (splitIntoTwo " = ")
        |> Seq.map (fun (left, right) ->
            let source = left.Trim()
            let destination = right.Trim().Replace("(", "").Replace(")", "")
            let destinationL, destinationR = splitIntoTwo "," destination
            source, (destinationL.Trim(), destinationR.Trim())
        )
        |> dict

    { directions = header; maps = maps }

let repeat items = 
    seq { while true do yield! items }

let traverse (map: Map) startingPosition =
    seq {
        let mutable position = startingPosition
        let mutable i = 0
        while true do
            for direction in repeat map.directions do
                match direction with
                | 'L' -> position <- fst (map.maps.[position])
                | 'R' -> position <- snd (map.maps.[position])
                | _ -> failwith "Invalid direction"

                i <- i + 1
                yield (i, position)
    }
    |> Seq.find (fun (_, position) -> isEndingPosition position)

let primeFactors n =
    let rec primeFactors' n x a = 
        if x = n then
            x::a
        elif n % x = 0 then
            primeFactors' (n / x) x (x::a)
        else
            primeFactors' n (x + 1) a
    primeFactors' n 2 []

let map = 
    readInputFile()
    |> parse

let startingPositions = 
    map.maps.Keys
    |> Seq.filter isStartingPosition
    |> Seq.toArray

startingPositions
|> Array.map (traverse map)
|> Array.map fst
|> Seq.collect (primeFactors)
|> Seq.distinct
|> Seq.map (int64)
|> Seq.reduce (*)