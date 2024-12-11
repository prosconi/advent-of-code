// --- Part Two ---
// The Historians sure are taking a long time. To be fair, the infinite corridors are very large.

// How many stones would you have after blinking a total of 75 times?

open System.IO
open System.Collections.Generic

let example = "0 1 10 99 999"

let fileLine = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "Day11.txt"))

let data = 
    fileLine
    |> _.Split(' ')
    |> List.ofArray
    |> List.map int64

let isEvenNumberOfDigits n = 
    (string n).Length % 2 = 0

let splitNumber n = 
    let str = string n
    [
        str.Substring(0, str.Length / 2) |> int64
        str.Substring(str.Length / 2, str.Length / 2) |> int64
    ]

let rule stone =
    if stone = 0L then [1L]
    elif isEvenNumberOfDigits stone then splitNumber stone
    else [stone * 2024L]

let blink (d: Dictionary<int64,int64>) =
    for kvp in d |> Seq.toArray do
        let v = kvp.Value
        if v > 0 then
            d[kvp.Key] <- d[kvp.Key] - v
            for item in rule kvp.Key do
                match d.ContainsKey item with
                | true -> d[item] <- d[item] + v
                | false -> d.Add(item, v)

let result =
    let d =
        data
        |> Seq.groupBy id
        |> Seq.map (fun (k, v) -> k, v |> Seq.length |> int64)
        |> dict
        |> Dictionary

    for _ = 1 to 75 do 
        blink d

    d
    |> Seq.toArray
    |> Seq.sumBy _.Value
