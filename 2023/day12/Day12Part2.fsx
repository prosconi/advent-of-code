// --- Part Two ---
// As you look out at the field of springs, you feel like there are way more springs than the condition records list. When you examine the records, you discover that they were actually folded up this whole time!

// To unfold the records, on each row, replace the list of spring conditions with five copies of itself (separated by ?) and replace the list of contiguous groups of damaged springs with five copies of itself (separated by ,).

// So, this row:

// .# 1
// Would become:

// .#?.#?.#?.#?.# 1,1,1,1,1
// The first line of the above example would become:

// ???.###????.###????.###????.###????.### 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3
// In the above example, after unfolding, the number of possible arrangements for some rows is now much larger:

// ???.### 1,1,3 - 1 arrangement
// .??..??...?##. 1,1,3 - 16384 arrangements
// ?#?#?#?#?#?#?#? 1,3,1,6 - 1 arrangement
// ????.#...#... 4,1,1 - 16 arrangements
// ????.######..#####. 1,6,5 - 2500 arrangements
// ?###???????? 3,2,1 - 506250 arrangements
// After unfolding, adding all of the possible arrangement counts together produces 525152.

// Unfold your condition records; what is the new sum of possible arrangement counts?

open System.IO

let example() =
    [|
        "#.#.### 1,1,3"
        ".#...#....###. 1,1,3"
        ".#.###.#.###### 1,3,1,6"
        "####.#...#... 4,1,1"
        "#....######..#####. 1,6,5"
        ".###.##....# 3,2,1"
    |]

let exampleWithUnknown() =
    [|
        "???.### 1,1,3"
        ".??..??...?##. 1,1,3"
        "?#?#?#?#?#?#?#? 1,3,1,6"
        "????.#...#... 4,1,1"
        "????.######..#####. 1,6,5"
        "?###???????? 3,2,1"
    |]

let readInputFile() =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Day12.txt"))
    
let splitIntoTwo (delimiter: string) (line: string) =
    let index = line.IndexOf(delimiter)
    let left = line.Substring(0, index)
    let right = line.Substring(index + delimiter.Length)
    left, right

let parseLine line =
    let left, right = splitIntoTwo " " line
    let left = left.Trim()
    let right = right.Trim().Split(',') |> Seq.map int |> Seq.toArray
    left, right

let parse lines =
    lines
    |> Seq.map parseLine
    |> Seq.toArray

let items = [ '#'; '.' ]

let rec generateAllPossibleCombinations = 
    function
    | 0 -> [[]]
    | length ->
        [ for item in items do 
            for rest in generateAllPossibleCombinations (length - 1) do 
                item :: rest ]

let group items =
    seq {
        let mutable v = None
        let mutable count = 0
        for item in items do
            let isChanged = 
                match v with
                | None -> v <- Some item; false; 
                | Some x -> x <> item

            if isChanged then
                yield v.Value, count
                count <- 0
                v <- Some item
            count <- count + 1

        yield v.Value, count
    }

let isSolution(items: string, numbers) =
    items
    |> group 
    |> Seq.filter (fst >> (=)'#')
    |> Seq.map snd
    |> Seq.toArray
    |> (=)numbers

// isSolution("#.#.###", [|1;1;3|]) // true
// isSolution("#.#.###", [|1;1;2|]) // false
// isSolution(".#.###.#.######", [|1;3;1;6|]) // true

let replaceFirstInstance (text: string) (findChar: char) (replaceChar: char) =
    let idx = text.IndexOf(findChar)
    let left = text.Substring(0, idx)
    let right = text.Substring(idx + 1)
    $"{left}{replaceChar}{right}"

let solution lines =
    seq {
        for (data, counts) in lines do
            let questionCount = data |> String.filter ((=)'?') |> Seq.length

            printfn "Processing: %s, questionCount: %d, permutationCount: %A" data questionCount (System.Math.Pow(2.0, float questionCount))

            for combination in generateAllPossibleCombinations questionCount do
                let mutable str = data
                for c in combination do str <- replaceFirstInstance str '?' c
                yield str, counts, isSolution(str, counts)
    }

let solution2 lines =
    lines
    |> solution
    |> Seq.filter (fun (_, _, isSolution) -> isSolution)
    |> Seq.map (fun (data, counts, _) -> 
        seq { 1..5 } |> Seq.map (fun _ -> data) |> String.concat ",",
        seq { 1..5 } |> Seq.collect (fun _ -> counts) |> Seq.toArray
    )
    |> solution

exampleWithUnknown()
|> parse
|> solution2
|> Seq.map (fun (_, _, x) -> x)
|> Seq.countBy ((=)true)
