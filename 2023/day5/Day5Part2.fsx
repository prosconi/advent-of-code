// Everyone will starve if you only plant such a small number of seeds. Re-reading the almanac, it looks like the seeds: line actually describes ranges of seed numbers.
// --- Part Two ---

// The values on the initial seeds: line come in pairs. Within each pair, the first value is the start of the range and the second value is the length of the range. So, in the first line of the example above:

// seeds: 79 14 55 13
// This line describes two ranges of seed numbers to be planted in the garden. The first range starts with seed number 79 and contains 14 values: 79, 80, ..., 91, 92. The second range starts with seed number 55 and contains 13 values: 55, 56, ..., 66, 67.

// Now, rather than considering four seed numbers, you need to consider a total of 27 seed numbers.

// In the above example, the lowest location number can be obtained from seed number 82, which corresponds to soil 84, fertilizer 84, water 84, light 77, temperature 45, humidity 46, and location 46. So, the lowest location number is 46.

// Consider all of the initial seed numbers listed in the ranges on the first line of the almanac. What is the lowest location number that corresponds to any of the initial seed numbers?

#r "nuget: Spectre.Console"

open System
open System.IO
open Spectre.Console
open System.Collections.Generic

let example() =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Example5.txt"))

let readInputFile() =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Day5.txt"))

type ListItem =
    {
        SourceRangeStart: int64
        DestinationRangeStart: int64
        Length: int64
    }

type SeedItem =
    {
        Seed: int64
        Length: int64
    }

// humidity-to-location map:
type ParsedData =
    {
        SeedToSoil: ListItem list
        SoilToFertilizer: ListItem list
        FertilizerToWater: ListItem list
        WaterToLight: ListItem list
        LightToTemperature: ListItem list
        TemperatureToHumidity: ListItem list
        HumidityToLocation: ListItem list
        Seeds: SeedItem[]
    }

let splitIntoTwo (line: string, delimiter: string) =
    let index = line.IndexOf(delimiter)
    let left = line.Substring(0, index)
    let right = line.Substring(index + delimiter.Length)
    left, right

let parseList (str: string) =
    ' '
    |> str.Split
    |> Seq.filter (not << String.IsNullOrWhiteSpace)
    |> Seq.map int64
    |> Seq.toList

let (|StartsWith|_|) (prefix:string) (s:string) =
    if s.StartsWith(prefix)
    then Some(s.Substring(prefix.Length))
    else None

let (|Map|_|) (s:string) =
    if s.Contains "-to-" && s.EndsWith ":"
    then Some(splitIntoTwo(s.Replace("map:", "").Trim(), "-to-"))
    else None

let (|Empty|_|) (s:string) =
    if String.IsNullOrWhiteSpace s
    then Some()
    else None

let (|List|_|) (s:string) =
    match parseList s with
    | [ destination; source; length ] -> 
        { DestinationRangeStart = destination; SourceRangeStart = source; Length = length }
        |> Some
    | _ -> 
        None

let (|Seeds|_|) (s:string) =
    match s with
    | StartsWith "seeds:" rest -> 
        rest
        |> parseList
        |> Seq.chunkBySize 2
        |> Seq.map (fun x -> { Seed = x[0]; Length = x[1] })
        |> Seq.toArray
        |> Some
    | _ -> 
        None

let parseLines (lines: string seq) =
    let mutable key = ""
    let mutable parsedData : ParsedData = 
        { SeedToSoil = []
          SoilToFertilizer = []
          FertilizerToWater = []
          WaterToLight = []
          LightToTemperature = []
          TemperatureToHumidity = []
          Seeds = [||]
          HumidityToLocation = [] }

    for line in lines do
        let line = line.Trim()

        match line with
        | Seeds s -> 
            parsedData <- { parsedData with Seeds = s }
        | Map (fromString, toString) -> 
            key <- $"{fromString}-to-{toString}"
        | Empty -> 
            ()
        | List items -> 
            match key with
            | "seed-to-soil"            -> parsedData <- { parsedData with SeedToSoil            = items :: parsedData.SeedToSoil }
            | "soil-to-fertilizer"      -> parsedData <- { parsedData with SoilToFertilizer      = items :: parsedData.SoilToFertilizer }
            | "fertilizer-to-water"     -> parsedData <- { parsedData with FertilizerToWater     = items :: parsedData.FertilizerToWater }
            | "water-to-light"          -> parsedData <- { parsedData with WaterToLight          = items :: parsedData.WaterToLight }
            | "light-to-temperature"    -> parsedData <- { parsedData with LightToTemperature    = items :: parsedData.LightToTemperature }
            | "temperature-to-humidity" -> parsedData <- { parsedData with TemperatureToHumidity = items :: parsedData.TemperatureToHumidity }
            | "humidity-to-location"    -> parsedData <- { parsedData with HumidityToLocation    = items :: parsedData.HumidityToLocation }
            | _ -> failwithf "Invalid key: %s" key
        | _ -> failwithf "Invalid line: %s" line
    parsedData

let lookup (list: ListItem list) (key: int64) =
    list
    |> Seq.tryFind (fun x -> key >= x.SourceRangeStart && key < x.SourceRangeStart + x.Length)
    |> Option.map (fun x -> key - (x.SourceRangeStart - x.DestinationRangeStart))
    |> Option.defaultValue key

let traverseMaps (parsedData: ParsedData) seed =
    seed
    |> lookup parsedData.SeedToSoil
    |> lookup parsedData.SoilToFertilizer
    |> lookup parsedData.FertilizerToWater
    |> lookup parsedData.WaterToLight
    |> lookup parsedData.LightToTemperature
    |> lookup parsedData.TemperatureToHumidity
    |> lookup parsedData.HumidityToLocation

let lockObj = obj()

let log str =
    lock lockObj (fun _ -> printfn "%s" str)

let table = 
    Table().AddColumns("Task #", "Percent Complete")

let tableDict = Dictionary<int, float>()

let refreshTable() =
    AnsiConsole.Live(table)
        .Start(fun ctx ->
            while true do
                table.Rows.Clear()
                for kvp in tableDict |> Seq.sortBy (fun x -> x.Key) do
                    table.AddRow(string kvp.Key, String.Format("{0:P2}", kvp.Value))
                    |> ignore
                ctx.Refresh()
                System.Threading.Thread.Sleep(1000)
        )

let go (parsedData: ParsedData) =
    parsedData.Seeds
    |> Seq.mapi (fun seedIndex s ->
        System.Threading.Tasks.Task.Run(fun _ ->
            seq { s.Seed .. s.Seed + s.Length - 1L }
            |> Seq.mapi (fun i x -> 
                if i % 100_000 = 0 then
                    tableDict[seedIndex] <- float i / (float s.Length)
                elif int64 i = s.Length then
                    tableDict[seedIndex] <- 1.0
                x
            )
            |> Seq.map (traverseMaps parsedData)
            |> Seq.min
        )
    )
    |> Seq.toArray

let startCalc() =

    let tasks = 
        readInputFile()
        |> parseLines
        |> go

    let _ = System.Threading.Tasks.Task.Run(fun _ -> refreshTable())
    tasks


let tasks = startCalc()

tasks
|> Array.map (fun task -> task.Result)
|> Array.min