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
    {| AvailablePatterns = lines[0].Split(',') |> Array.map (fun x -> x.Trim()) |> Array.sortByDescending _.Length
       DisplayPatterns = lines[2..] |}

let rec isValid path (availablePatterns: string[]) (displayPattern: string) =
    match displayPattern.Length with
    | 0 -> 
        true
    | _ ->
        let patternMatches = 
            availablePatterns
            |> Array.filter (fun x -> displayPattern.StartsWith(x))

        match patternMatches with
        | [||] -> 
            false
        | _ ->
            patternMatches
            |> Seq.tryFind (fun p ->
                let newPattern = displayPattern.Substring(p.Length)
                isValid (p :: path) availablePatterns newPattern
            )
            |> Option.isSome

data.DisplayPatterns
|> Array.map (fun x -> printfn "Finding... %s" x; isValid [] data.AvailablePatterns x)
|> Array.filter (not)
|> Array.length
