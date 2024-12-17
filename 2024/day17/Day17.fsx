open System
open System.IO

let example =
    [|
        "Register A: 729"
        "Register B: 0"
        "Register C: 0"
        ""
        "Program: 0,1,5,4,3,0"
    |]

let splitIntoTwo (delimiter: string) (line: string) =
    match line.Split(delimiter) with
    | [| left; right |] -> left, right
    | _ -> failwithf "Invalid line: %s" line

let readInputFile() =
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Day17.txt"))

let mutable A, B, C, IP = 0, 0, 0, 0
let mutable PROG = [|0|]
let output = ResizeArray<string>()

let data = 
    match readInputFile() with
    | [|a; b; c; _blank; program|] ->
        A <- splitIntoTwo ":" a |> snd |> int
        B <- splitIntoTwo ":" b |> snd |> int
        C <- splitIntoTwo ":" c |> snd |> int
        PROG <- splitIntoTwo ":" program |> snd |> _.Split(',') |> Array.map int
    | other -> failwithf "Invalid program: %A" other

let getComboOperand(cb) =
    match cb with
    | 4 -> A
    | 5 -> B
    | 6 -> C
    | 7 -> failwithf "Invalid combo operand: %d" cb
    | _ -> cb

let incrementIP() =
    IP <- IP + 2

let adv operand = 
    let cb = getComboOperand operand
    let denominator = int <| Math.Pow(2., float cb)
    A <- A / denominator
    incrementIP()

let bxl operand =
    B <- B ^^^ operand
    incrementIP()

let bst operand =
    let cb = getComboOperand operand
    B <- cb % 8
    incrementIP()

let jnz operand =
    match A with
    | 0 -> incrementIP()
    | _ -> IP <- operand

let bxc operand =
    B <- B ^^^ C
    incrementIP()

let out operand =
    let cb = getComboOperand operand
    let v = cb % 8
    output.Add(v.ToString())
    incrementIP()

let bdv operand =
    let cb = getComboOperand operand
    let denominator = int <| Math.Pow(2., float cb)
    B <- A / denominator
    incrementIP()

let cdv operand =
    let cb = getComboOperand operand
    let denominator = int <| Math.Pow(2., float cb)
    C <- A / denominator
    incrementIP()

let runProgram() =
    while IP < PROG.Length do
        let opcode = PROG[IP]
        let operand = PROG[IP + 1]
        match opcode with
        | 0 -> adv operand
        | 1 -> bxl operand
        | 2 -> bst operand
        | 3 -> jnz operand
        | 4 -> bxc operand
        | 5 -> out operand
        | 6 -> bdv operand
        | 7 -> cdv operand
        | _ -> failwithf "Invalid opcode: %d" opcode
    output |> String.concat ","

runProgram()