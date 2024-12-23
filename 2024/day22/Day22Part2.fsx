#nowarn "00025"

open System
open System.IO

let readInputFile() =
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Day22.txt"))

let example =
    [ "1"
      "10"
      "100"
      "2024" ]

let lines = readInputFile()

let divideRoundDown a b =
    Math.Floor(float a / float b)
    |> int64

let calculate (secretNumber: int64) =
    let secretNumber = ((secretNumber * 64L) ^^^ secretNumber) % 16777216L
    let secretNumber = ((divideRoundDown secretNumber 32L) ^^^ secretNumber) % 16777216L
    let secretNumber = ((secretNumber * 2048L) ^^^ secretNumber) % 16777216L
    secretNumber

let getOnesDigit secretNumber = 
    secretNumber % 10L

let calc numberOfTimes secretNumber =
    seq {
        let mutable i = secretNumber
        i, getOnesDigit i
        for _ in seq { 1 .. numberOfTimes } do
            i <- calculate i
            i, getOnesDigit i
    }

let allCaches =
    lines
    |> Seq.map int64
    |> Seq.map (calc 2000)
    |> Seq.map (fun x ->
        x
        |> Seq.windowed 2
        |> Seq.map (fun [|a; b|] -> a, b)
        |> Seq.map (fun ((secretNumber1, price1), (secretNumber2, price2)) ->
            {| SecretNumber = secretNumber2; Price = price2; DeltaPrice = price2 - price1 |}
        )
        |> Seq.windowed 4
        |> Seq.groupBy (fun [|a;b;c;d|] -> $"{a.DeltaPrice}|{b.DeltaPrice}|{c.DeltaPrice}|{d.DeltaPrice}")
        |> Seq.map (fun (key, values) -> key, values |> Seq.map (Seq.last) |> Seq.head |> _.Price)
        |> dict
    )
    |> Seq.toArray

let allKeys =
    allCaches
    |> Seq.collect (fun x -> x.Keys)
    |> Seq.distinct
    |> Seq.toArray

allKeys
|> Seq.indexed
|> Seq.map (fun (i,key) ->
    if i % 100 = 0 then printfn "Going for key %A" (float i / float allKeys.Length)
    key, allCaches
    |> Seq.choose (fun x -> 
        match x.TryGetValue key with
        | true, value -> Some value
        | false, _ -> None
    )
    |> Seq.sum
)
|> Seq.maxBy snd