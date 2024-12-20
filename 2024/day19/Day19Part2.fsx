open System.IO
open System.Collections.Generic

let example =
    [|
        "r, wr, b, g, bwu, rb, gb, br"
        ""
        "brwrr"
        "bggr"
        "gbbr"
        "rrbgbr"
        "ubwu"
        "bwurrg"
        "brgr"
        "bbrgwb"
    |]

let readInputFile() =
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Day19.txt"))

let data = 
    // let lines = example
    let lines = readInputFile()
    {| AvailablePatterns = lines[0].Split(',') |> Array.map (fun x -> x.Trim()) |> Array.distinct
       DisplayPatterns = lines[2..] |}

let isValid initialPattern =
    let cache = Dictionary()

    let rec isValid' pattern =
        match pattern with
        | "" -> 1L
        | _ ->
            match cache.TryGetValue(pattern) with
            | true, v -> v
            | _ ->
                let mutable total = 0L
                for i = 1 to pattern.Length do
                    let s = pattern.Substring(0, i)
                    if data.AvailablePatterns |> Array.exists ((=)s) then
                        total <- total + isValid' (pattern.Substring(i))
                cache[pattern] <- total
                total

    isValid' initialPattern

data.DisplayPatterns
|> Array.map (fun x -> printfn "Finding... %s" x; isValid x)
|> Array.sum
