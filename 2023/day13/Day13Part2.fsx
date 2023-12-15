// --- Part Two ---
// You resume walking through the valley of mirrors and - SMACK! - run directly into one. Hopefully nobody was watching, because that must have been pretty embarrassing.

// Upon closer inspection, you discover that every mirror has exactly one smudge: exactly one . or # should be the opposite type.

// In each pattern, you'll need to locate and fix the smudge that causes a different reflection line to be valid. (The old reflection line won't necessarily continue being valid after the smudge is fixed.)

// Here's the above example again:

// #.##..##.
// ..#.##.#.
// ##......#
// ##......#
// ..#.##.#.
// ..##..##.
// #.#.##.#.

// #...##..#
// #....#..#
// ..##..###
// #####.##.
// #####.##.
// ..##..###
// #....#..#
// The first pattern's smudge is in the top-left corner. If the top-left # were instead ., it would have a different, horizontal line of reflection:

// 1 ..##..##. 1
// 2 ..#.##.#. 2
// 3v##......#v3
// 4^##......#^4
// 5 ..#.##.#. 5
// 6 ..##..##. 6
// 7 #.#.##.#. 7
// With the smudge in the top-left corner repaired, a new horizontal line of reflection between rows 3 and 4 now exists. Row 7 has no corresponding reflected row and can be ignored, but every other row matches exactly: row 1 matches row 6, row 2 matches row 5, and row 3 matches row 4.

// In the second pattern, the smudge can be fixed by changing the fifth symbol on row 2 from . to #:

// 1v#...##..#v1
// 2^#...##..#^2
// 3 ..##..### 3
// 4 #####.##. 4
// 5 #####.##. 5
// 6 ..##..### 6
// 7 #....#..# 7
// Now, the pattern has a different horizontal line of reflection between rows 1 and 2.

// Summarize your notes as before, but instead use the new different reflection lines. In this example, the first pattern's new horizontal line has 3 rows above it and the second pattern's new horizontal line has 1 row above it, summarizing to the value 400.

// In each pattern, fix the smudge and find the different line of reflection. What number do you get after summarizing the new reflection line in each pattern in your notes?


open System.IO

let readInputFile() =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Day13.txt"))
    
let example() =
    [|
        "#.##..##."
        "..#.##.#."
        "##......#"
        "##......#"
        "..#.##.#."
        "..##..##."
        "#.#.##.#."
        ""
        "#...##..#"
        "#....#..#"
        "..##..###"
        "#####.##."
        "#####.##."
        "..##..###"
        "#....#..#"
    |]

let partition lines =
    [|
        let lst = ResizeArray()
        for line in lines do
            if line = "" then 
                yield lst.ToArray()
                lst.Clear()
            else
                lst.Add(line)
                
        yield lst.ToArray()
    |] 

let rotate lines =
    let width = 0 |> Array.get lines |> String.length
    seq { 0 .. width - 1 }
    |> Seq.map (fun i -> lines |> Array.map (fun line -> line.[i]) |> System.String)
    |> Seq.toArray

let rec indexes delta length startIndex =
    let idx1 = startIndex + delta
    let idx2 = startIndex - delta - 1
    
    if idx1 >= length then []
    elif idx2 < 0 then []
    else (idx1, idx2) :: indexes (delta + 1) length startIndex

let findMirror(lines: string[]) =
    let m = 
        [1..lines.Length - 1]
        |> List.map (fun x -> x, indexes 0 lines.Length x)
        |> List.choose (fun (x, indexes) -> 
            let allTrue = 
                indexes
                |> List.map (fun (idx1, idx2) -> lines[idx1] = lines[idx2])
                |> List.forall id

            match allTrue with
            | true -> Some x
            | false -> None
        )

    match m with
    | [] -> None
    | [x] -> Some x
    | _ -> failwithf "Multiple mirrors found in %A" lines

let replaceEveryChar (line: string) =
    seq {
        for i = 0 to line.Length - 1 do
            match line[i] with
            | '#' -> line.Remove(i, 1).Insert(i, ".")
            | '.' -> line.Remove(i, 1).Insert(i, "#")
            | other -> failwithf "Unexpected character %c" other
    }

let fixSmudgesMinMax min max (lines: string[]) =
    [|
        if min < 0 then failwithf "min=%d" min
        if max >= lines.Length then failwithf "max=%d, length=%d" max lines.Length
        for i = min to max do
            let line = lines.[i]
            for newLine in replaceEveryChar line do
                let newArray = Array.init lines.Length (fun i -> lines[i])
                newArray[i] <- newLine
                yield newArray
    |]

// let lines = "123456789"
// let line = 4
// let min, max = 
//     let mid = lines.Length / 2
//     if line <= mid
//     then 0, line + line - 1
//     else line - (lines.Length - line), lines.Length - 1

let findMirrors lines =
    let rotated = lines |> rotate
    let f1 = lines |> findMirror
    let f2 = rotated |> findMirror

    let (min1, max1), (min2, max2) = 
        match f1, f2 with
        | None, None -> failwithf "Unable to find mirror in %A" lines
        | Some x, Some y -> failwithf "Both horizontal and vertical mirrors found: %d, %d" x y
        | Some x, None -> 
            let min, max = 
                let mid = lines.Length / 2
                if x <= mid
                then 0, x + x
                else x - (lines.Length - x), lines.Length - 1

            (min, max), (0, rotated.Length)
        | None, Some x -> 
            let min, max = 
                let mid = rotated.Length / 2
                if x <= mid
                then 0, x + x
                else x - (rotated.Length - x), rotated.Length - 1

            (0, lines.Length), (min, max)

    let f1 =
        lines
        |> fixSmudgesMinMax min1 max1
        |> Array.map findMirror
        |> Array.tryFind Option.isSome
        |> Option.defaultValue None
    
    let f2 =
        rotated
        |> fixSmudgesMinMax min2 max2
        |> Array.map findMirror
        |> Array.tryFind Option.isSome
        |> Option.defaultValue None

    match f1, f2 with
    | None, None -> failwithf "Unable to find mirror in %A" lines
    | Some f1, Some f2 -> failwithf "Both horizontal and vertical mirrors found: %d, %d" f1 f2
    | Some f1, None -> f1 * 100
    | None, Some f2 -> f2


readInputFile()
|> partition
|> Array.map findMirrors
|> Array.sum
