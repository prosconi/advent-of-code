open System
open System.IO
open System.Collections.Generic

let readInputFile() =
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Day25.txt"))

let example =
    [|
        "#####"
        ".####"
        ".####"
        ".####"
        ".#.#."
        ".#..."
        "....."
        ""
        "#####"
        "##.##"
        ".#.##"
        "...##"
        "...#."
        "...#."
        "....."
        ""
        "....."
        "#...."
        "#...."
        "#...#"
        "#.#.#"
        "#.###"
        "#####"
        ""
        "....."
        "....."
        "#.#.."
        "###.."
        "###.#"
        "###.#"
        "#####"
        ""
        "....."
        "....."
        "....."
        "#...."
        "#.#.."
        "#.#.#"
        "#####"
    |]

let lines = example

let coords =
    lines
    |> Seq.chunkBySize 8
    |> Seq.map (fun m ->
        m
        |> Seq.takeWhile (fun l -> l <> "")
        |> Seq.indexed
        |> Seq.map (fun (y, l) ->
            l
            |> Seq.indexed
            |> Seq.choose (fun (x, c) -> if c = '#' then Some (x, y + 1) else None)
            |> Seq.toArray
        )
        |> Seq.toArray
    )
    |> Seq.toArray

let locks = 
    coords
    |> Seq.filter (fun x -> 
        x
        |> Seq.collect id
        |> Seq.groupBy snd
        |> Seq.length = 6
    )
    |> Seq.map (fun x ->
        x
        |> Seq.collect id
        |> Seq.groupBy fst
        |> Seq.toArray
        |> Seq.map (fun (k, v) -> (k, v |> Seq.map snd |> Seq.max))
        |> Seq.toArray
        |> dict
    )
    |> Seq.toArray

locks |> Seq.head |> Seq.iter (fun x -> printf "%A " x); printfn ""

let keys = 
    coords
    |> Seq.map (fun x ->
        x
        |> Seq.collect id
        |> Seq.groupBy fst
        |> Seq.toArray
        |> Seq.map (fun (k, v) -> (k, v |> Seq.map snd |> Seq.min))
        |> Seq.toArray
        |> dict
    )
    |> Seq.toArray

seq {
    for x1, lock in locks |> Seq.indexed do
        for x2, key in keys |> Seq.indexed do
            if x1 <> x2 then
                for i = 0 to 4 do
                    match lock.TryGetValue i, key.TryGetValue i with
                    | (true, v1), (true, v2) ->
                        if v1 + v2 <= 5
                        then printfn "%d + %d <= 5 - %A %A" v1 v2 lock key; 1
                        else 0
                    | _ -> 0  
}
|> Seq.sum