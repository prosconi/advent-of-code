open System
open System.IO

let example =
    [|
        "Register A: 2024"
        "Register B: 0"
        "Register C: 0"
        ""
        "Program: 0,3,5,4,3,0"
    |]

let splitIntoTwo (delimiter: string) (line: string) =
    match line.Split(delimiter) with
    | [| left; right |] -> left, right
    | _ -> failwithf "Invalid line: %s" line

let inputFileData =
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Day17.txt"))

let mutable A, B, C, IP = 0L, 0L, 0L, 0
let mutable PROG = [|0|]
let mutable output = ResizeArray<int>()

let loadData() = 
    match inputFileData with
    | [|a; b; c; _blank; program|] ->
        A <- splitIntoTwo ":" a |> snd |> int64
        B <- splitIntoTwo ":" b |> snd |> int64
        C <- splitIntoTwo ":" c |> snd |> int64
        IP <- 0
        PROG <- splitIntoTwo ":" program |> snd |> _.Split(',') |> Array.map int
        output <- ResizeArray<int>()
    | other -> failwithf "Invalid program: %A" other

let getComboOperand(cb) =
    match int cb with
    | 4 -> A
    | 5 -> B
    | 6 -> C
    | 7 -> failwithf "Invalid combo operand: %d" cb
    | _ -> cb

let incrementIP() =
    IP <- IP + 2

let adv operand = 
    let cb = getComboOperand operand
    let denominator = int64 <| Math.Pow(2., float cb)
    A <- A / denominator
    incrementIP()

let bxl operand =
    B <- B ^^^ operand
    incrementIP()

let bst operand =
    let cb = getComboOperand operand
    B <- cb % 8L
    incrementIP()

let jnz operand =
    match int A with
    | 0 -> incrementIP()
    | _ -> IP <- operand

let bxc operand =
    B <- B ^^^ C
    incrementIP()

let out operand =
    let cb = getComboOperand operand
    let v = cb % 8L
    output.Add(int v)
    incrementIP()

let bdv operand =
    let cb = getComboOperand operand
    let denominator = int64 <| Math.Pow(2., float cb)
    B <- A / denominator
    incrementIP()

let cdv operand =
    let cb = getComboOperand operand
    let denominator = int64 <| Math.Pow(2., float cb)
    C <- A / denominator
    incrementIP()

let runProgram overrideA =
    loadData()
    A <- overrideA

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
    
    output |> Seq.toArray, PROG

let rec search(currentValue) =
    for i in 0L..7L do
        let overrideA = currentValue * 8L + i
        let output, program = runProgram overrideA
        
        if output = program then 
            failwithf "Found it: %d" overrideA

        let strOutput = output |> Seq.map string |> String.concat ""
        let strProgram = program |> Seq.map string |> String.concat ""
        if strProgram.EndsWith strOutput then
            printfn "search (%d): %s = %s" overrideA strOutput strProgram
            search(overrideA)

search(0)
