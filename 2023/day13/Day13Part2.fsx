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

let rec indexes delta length startIndex =
    let idx1 = startIndex + delta
    let idx2 = startIndex - delta - 1
    
    if idx1 >= length then []
    elif idx2 < 0 then []
    else (idx1, idx2) :: indexes (delta + 1) length startIndex

let getVerticalString (lines: string[]) index =
    lines
    |> Array.map (fun line -> line[index])
    |> System.String

let findMirrorLeftRight(lines: string[]) =
    let width = lines[0].Length
    let m = 
        [1..width - 1]
        |> List.map (fun x -> x, indexes 0 width x)
        |> List.choose (fun (x, indexes) -> 
            let minMax = indexes |> List.last
            let allTrue = 
                indexes
                |> List.map (fun (idx1, idx2) -> 
                    let line1 = getVerticalString lines idx1
                    let line2 = getVerticalString lines idx2
                    line1 = line2
                )
                |> List.forall id

            match allTrue with
            | true -> Some (minMax, lines, x)
            | false -> None
        )

    match m with
    | [] -> None
    | [x] -> Some x
    | _ -> failwithf "Multiple mirrors found in %A" lines

let findMirrorUpDown(lines: string[]) =
    let height = lines.Length
    let m = 
        [1..height - 1]
        |> List.map (fun x -> x, indexes 0 height x)
        |> List.choose (fun (x, indexes) -> 
            let minMax = indexes |> List.last
            let allTrue = 
                indexes
                |> List.map (fun (idx1, idx2) -> lines[idx1] = lines[idx2])
                |> List.forall id

            match allTrue with
            | true -> Some(minMax, lines, x)
            | false -> None
        )

    match m with
    | [] -> None
    | [x] -> Some x
    | _ -> failwithf "Multiple mirrors found in %A" lines

let replaceEveryChar (min, max) (line: string) =
    seq {
        for i = min to max do
            match line[i] with
            | '#' -> line.Remove(i, 1).Insert(i, ".")
            | '.' -> line.Remove(i, 1).Insert(i, "#")
            | other -> failwithf "Unexpected character %c" other
    }

let fixSmudges (lines: string[]) =
    [|
        for i = 0 to lines.Length - 1 do
            let line = lines.[i]
            for newLine in replaceEveryChar (0, line.Length - 1) line do
                let newArray = Array.init lines.Length (fun i -> lines[i])
                newArray[i] <- newLine
                yield newArray
    |]

let findMirrors lines =
    let upDown = lines |> findMirrorUpDown
    let leftRight = lines |> findMirrorLeftRight

    let newLines = 
        match upDown, leftRight with
        | None, None -> failwith "Unable to find mirror"
        | Some (_, _, x), Some (_, _, y) -> failwithf "Both horizontal and vertical mirrors found: %d, %d" x y
        | Some ((min, max), _, _), None -> 
            let min' = System.Math.Min(min, max)
            let max' = System.Math.Max(min, max)
            lines
            |> Seq.mapi (fun i x -> i, x)
            |> Seq.filter (fun (i, _) -> i >= min' && i <= max')
            |> Seq.map snd
            |> Seq.toArray
        | None, Some ((min, max), _, _) -> 
            let min' = System.Math.Min(min, max)
            let max' = System.Math.Max(min, max)
            lines
            |> Seq.map (fun line -> 
                line
                |> Seq.mapi (fun i c -> i, c)
                |> Seq.filter (fun (i, _) -> i >= min' && i <= max')
                |> Seq.map snd
                |> Seq.toArray
                |> System.String
            )
            |> Seq.toArray

    let upDownSmudge =
        newLines
        |> fixSmudges
        |> Array.map findMirrorUpDown
        |> Array.tryFind Option.isSome
        |> Option.defaultValue None
    
    let leftRightSmudge =
        newLines
        |> fixSmudges
        |> Array.map findMirrorLeftRight
        |> Array.tryFind Option.isSome
        |> Option.defaultValue None

    match upDownSmudge, leftRightSmudge with
    | None, None -> 
        printfn "upDown: %A - leftRight: %A" upDown leftRight
        printfn "--original--"
        lines |> Array.iter (printfn "%s")
        printfn ""
        printfn "--new--"
        newLines |> Array.iter (printfn "%s")
        failwithf "Unable to find mirror"
    | Some (_, lines1, f1), Some (_, lines2, f2) ->
        printfn "upDown: %A - leftRight: %A" upDown leftRight
        printfn "--original--"
        lines |> Array.iter (printfn "%s")
        printfn ""
        printfn "--new--"
        newLines |> Array.iter (printfn "%s")
        printfn ""
        printfn "--up/down--"
        lines1 |> Array.iter (printfn "%s")
        printfn ""
        printfn "--left/right--"
        lines2 |> Array.iter (printfn "%s")
        printfn ""
        failwithf "Both horizontal and vertical mirrors found: %d, %d" f1 f2
    | _, _ -> ()
    // | Some f1, None -> f1 * 100
    // | None, Some f2 -> f2


example()
|> partition
|> Array.last
