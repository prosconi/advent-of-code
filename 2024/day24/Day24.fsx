open System
open System.IO
open System.Collections.Generic

let readInputFile() =
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Day24.txt"))

let example =
    [|
        "x00: 1"
        "x01: 1"
        "x02: 1"
        "y00: 0"
        "y01: 1"
        "y02: 0"
        ""
        "x00 AND y00 -> z00"
        "x01 XOR y01 -> z01"
        "x02 OR y02 -> z02"
    |]

let example2 =
    [|
        "x00: 1"
        "x01: 0"
        "x02: 1"
        "x03: 1"
        "x04: 0"
        "y00: 1"
        "y01: 1"
        "y02: 1"
        "y03: 1"
        "y04: 1"
        ""
        "ntg XOR fgs -> mjb"
        "y02 OR x01 -> tnw"
        "kwq OR kpj -> z05"
        "x00 OR x03 -> fst"
        "tgd XOR rvg -> z01"
        "vdt OR tnw -> bfw"
        "bfw AND frj -> z10"
        "ffh OR nrd -> bqk"
        "y00 AND y03 -> djm"
        "y03 OR y00 -> psh"
        "bqk OR frj -> z08"
        "tnw OR fst -> frj"
        "gnj AND tgd -> z11"
        "bfw XOR mjb -> z00"
        "x03 OR x00 -> vdt"
        "gnj AND wpb -> z02"
        "x04 AND y00 -> kjc"
        "djm OR pbm -> qhw"
        "nrd AND vdt -> hwm"
        "kjc AND fst -> rvg"
        "y04 OR y02 -> fgs"
        "y01 AND x02 -> pbm"
        "ntg OR kjc -> kwq"
        "psh XOR fgs -> tgd"
        "qhw XOR tgd -> z09"
        "pbm OR djm -> kpj"
        "x03 XOR y03 -> ffh"
        "x00 XOR y04 -> ntg"
        "bfw OR bqk -> z06"
        "nrd XOR fgs -> wpb"
        "frj XOR qhw -> z04"
        "bqk OR frj -> z07"
        "y03 OR x01 -> nrd"
        "hwm AND bqk -> z03"
        "tgd XOR rvg -> z12"
        "tnw OR pbm -> gnj"
    |]

let lines = readInputFile()

let splitIntoTwo (delimiter: string) (line: string) =
    match line.Split(delimiter) with
    | [| left; right |] -> left.Trim(), right.Trim()
    | _ -> failwithf "Invalid line: %s" line

let vars, eqs =
    lines
    |> Array.filter (fun x -> x <> "")
    |> Array.partition (fun x -> x.Contains ":")

let eqsArray = eqs |> Array.map (fun x -> splitIntoTwo "->" x)
let varsDict = vars |> Array.map (fun x -> splitIntoTwo ":" x) |> Array.map (fun (x, y) -> x, int y = 1) |> dict |> Dictionary

let evalEq (leftEq: string, rightVar) =
    match varsDict.ContainsKey rightVar with
    | true -> None
    | false ->
        match leftEq.Split ' ' with
        | [|v1; op; v2|] -> 
            match varsDict.TryGetValue v1 with
            | true, v1 -> 
                match varsDict.TryGetValue v2 with
                | true, v2 ->
                    let v = 
                        match op with
                        | "AND" -> v1 && v2
                        | "OR" -> v1 || v2
                        | "XOR" -> v1 <> v2
                        | _ -> failwithf "Invalid op: %s" op
                    printfn "Setrting %s to %b" rightVar v
                    varsDict[rightVar] <- v
                    Some v
                | _ -> None
            | _ -> None
        | _ -> None

while eqsArray |> Array.forall (fun x -> evalEq x = None) = false do
    ()

varsDict
|> Seq.filter (fun x -> x.Key.StartsWith "z")
|> Seq.sortBy _.Key
|> Seq.map _.Value
|> Seq.map (fun x -> if x then "1" else "0")
|> Seq.rev
|> String.concat ""
|> fun str -> Convert.ToInt64(str, 2)