// --- Day 6: Trash Compactor ---
// After helping the Elves in the kitchen, you were taking a break and helping them re-enact a movie scene when you over-enthusiastically jumped into the garbage chute!

// A brief fall later, you find yourself in a garbage smasher. Unfortunately, the door's been magnetically sealed.

// As you try to find a way out, you are approached by a family of cephalopods! They're pretty sure they can get the door open, but it will take some time. While you wait, they're curious if you can help the youngest cephalopod with her math homework.

// Cephalopod math doesn't look that different from normal math. The math worksheet (your puzzle input) consists of a list of problems; each problem has a group of numbers that need to be either added (+) or multiplied (*) together.

// However, the problems are arranged a little strangely; they seem to be presented next to each other in a very long horizontal list. For example:

// 123 328  51 64 
//  45 64  387 23 
//   6 98  215 314
// *   +   *   +  
// Each problem's numbers are arranged vertically; at the bottom of the problem is the symbol for the operation that needs to be performed. Problems are separated by a full column of only spaces. The left/right alignment of numbers within each problem can be ignored.

// So, this worksheet contains four problems:

// 123 * 45 * 6 = 33210
// 328 + 64 + 98 = 490
// 51 * 387 * 215 = 4243455
// 64 + 23 + 314 = 401
// To check their work, cephalopod students are given the grand total of adding together all of the answers to the individual problems. In this worksheet, the grand total is 33210 + 490 + 4243455 + 401 = 4277556.

// Of course, the actual worksheet is much wider. You'll need to make sure to unroll it completely so that you can read the problems clearly.

// Solve the problems on the math worksheet. What is the grand total found by adding together all of the answers to the individual problems?

let exampleLines = 
    [|
        "123 328  51 64 "
        " 45 64  387 23 "
        "  6 98  215 314"
        "*   +   *   +  "
    |]

let fileLines = System.IO.File.ReadAllLines(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Day6.txt"))

let splitBySpaces (line: string) =
    line.Split ' ' |> Array.filter (fun s -> s <> "")

let parseLines (lines: string[]) =
    let numbers =
        lines
        |> Array.take (Array.length lines - 1)
        |> Array.map splitBySpaces
        |> Array.map (Array.map int64)

    let operations =
        lines
        |> Array.last
        |> splitBySpaces

    numbers, operations

let calcPart1 (numbers: int64[][], operations) =
    let width = Array.get numbers 0 |> Array.length
    let rotated = 
        numbers
        |> Array.transpose

    seq { 0 .. width - 1}
    |> Seq.map (fun i ->
        match Array.get operations i with
        | "+" -> Array.reduce (+) rotated[i]
        | "*" -> Array.reduce (*) rotated[i]
        | _ -> failwith "Unknown operator"
    )
    |> Seq.sum


let numbers, operations = parseLines fileLines
let part1 = calcPart1 (numbers, operations)

// --- Part Two ---
// The big cephalopods come back to check on how things are going. When they see that your grand total doesn't match the one expected by the worksheet, they realize they forgot to explain how to read cephalopod math.

// Cephalopod math is written right-to-left in columns. Each number is given in its own column, with the most significant digit at the top and the least significant digit at the bottom. (Problems are still separated with a column consisting only of spaces, and the symbol at the bottom of the problem is still the operator to use.)

// Here's the example worksheet again:

// 123 328  51 64 
//  45 64  387 23 
//   6 98  215 314
// *   +   *   +  
// Reading the problems right-to-left one column at a time, the problems are now quite different:

// The rightmost problem is 4 + 431 + 623 = 1058
// The second problem from the right is 175 * 581 * 32 = 3253600
// The third problem from the right is 8 + 248 + 369 = 625
// Finally, the leftmost problem is 356 * 24 * 1 = 8544
// Now, the grand total is 1058 + 3253600 + 625 + 8544 = 3263827.

// Solve the problems on the math worksheet again. What is the grand total found by adding together all of the answers to the individual problems?

let calcPart2 (lines: string[]) =
    let indexedOps =
        lines
        |> Array.last
        |> Seq.mapi (fun i c -> i, c)
        |> Seq.choose (fun (i, c) -> if c <> ' ' then Some (i, c) else None)
        |> Seq.toArray

    let ranges =
        indexedOps
        |> Seq.windowed 2
        |> Seq.map (fun [|(idx1,_);(idx2,_)|] -> idx1, idx2 - 1)
        |> Seq.toArray

    let lastRange = ranges |> Array.last
    let ranges =
        Array.append ranges [| (snd lastRange + 1, lines[0].Length) |]
    let numbers =
        lines
        |> Array.take (Array.length lines - 1)
        |> Array.map (fun line ->
            ranges
            |> Array.map (fun (startIdx, endIdx) -> line[startIdx..endIdx - 1])
        )
        |> Array.transpose

    let realNumbers =
        numbers
        |> Array.map (fun nums ->
            let width = Array.get nums 0 |> String.length
            
            seq { width - 1 .. -1 .. 0 }            
            |> Seq.map (fun idx -> 
                nums
                |> Seq.choose (fun str ->
                    match str[idx] with
                    | ' ' -> None
                    | c -> Some c
                )
                |> Seq.map string
                |> String.concat ""
            )
        )

    realNumbers
    |> Array.mapi (fun i nums ->
        match snd indexedOps[i] with
        | '+' -> nums |> Seq.map int64 |> Seq.sum
        | '*' -> nums |> Seq.map int64 |> Seq.reduce (*)
        | _ -> failwith "Unknown operator"
    )
    |> Seq.sum

let part2 = calcPart2 fileLines

