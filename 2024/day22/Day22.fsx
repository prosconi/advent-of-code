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

let calc numberOfTimes secretNumber =
    let mutable i = secretNumber
    for _ in seq { 1 .. numberOfTimes } do
        i <- calculate i
    i

lines
|> Seq.map int64
|> Seq.map (calc 2000)
|> Seq.sum 

calc 2000 1