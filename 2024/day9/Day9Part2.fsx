// --- Part Two ---
// Upon completion, two things immediately become clear. First, the disk definitely has a lot more contiguous free space, just like the amphipod hoped. Second, the computer is running much more slowly! Maybe introducing all of that file system fragmentation was a bad idea?

// The eager amphipod already has a new plan: rather than move individual blocks, he'd like to try compacting the files on his disk by moving whole files instead.

// This time, attempt to move whole files to the leftmost span of free space blocks that could fit the file. Attempt to move each file exactly once in order of decreasing file ID number starting with the file with the highest file ID number. If there is no span of free space to the left of a file that is large enough to fit the file, the file does not move.

// The first example from above now proceeds differently:

// 00...111...2...333.44.5555.6666.777.888899
// 0099.111...2...333.44.5555.6666.777.8888..
// 0099.1117772...333.44.5555.6666.....8888..
// 0099.111777244.333....5555.6666.....8888..
// 00992111777.44.333....5555.6666.....8888..
// The process of updating the filesystem checksum is the same; now, this example's checksum would be 2858.

// Start over, now compacting the amphipod's hard drive using this new method instead. What is the resulting filesystem checksum?
open System.IO

let exampleLine1 = "2333133121414131402"
let exampleLine2 = "12345"
let fileLine = 
    Path.Combine(__SOURCE_DIRECTORY__, "Day9.txt")
    |> File.ReadAllLines
    |> Seq.head

let charToInt (c: char)= c |> string |> int

let isCompact data =
    let firstDot = data |> Seq.findIndex (fun c -> c = ".")
    let lastDot = data |> Seq.findIndexBack (fun c -> c <> ".")
    firstDot - lastDot = 1

let swap (data: _[]) (i1, i2) =
    let temp = data[i1]
    data[i1] <- data[i2]
    data[i2] <- temp

let checksum data = 
    data
    |> Seq.indexed
    |> Seq.sumBy (fun (i, c) -> 
        if c = "."
        then 0L
        else int64 i * int64 c
    )

let findOpenSpot (data: string[]) length =
    let dots = String.replicate length "." 
    let str = data |> Seq.map (fun x -> x[0]) |> Seq.map string |> String.concat ""
    match str.IndexOf(dots) with
    | -1 -> None
    | i -> Some i

let swapIn (data: _[]) startIndex length openSpot =
    for i = startIndex to startIndex + length do
        let idx1 = i
        let idx2 = openSpot + i - startIndex
        swap data (idx1, idx2)

let lastIndexOfAny index fn (data: _[])  =
    seq {
        for i = index downto 0 do
            if fn(data[i])
            then Some i
            else None
    }
    |> Seq.choose id
    |> Seq.tryHead

let calc (data: string) =
    
    let asArray =
        data
        |> Seq.chunkBySize 2
        |> Seq.map (fun d -> 
            match d with
            | [| usedSpace; freeSpace |] -> charToInt usedSpace, charToInt freeSpace
            | [| usedSpace |] -> charToInt usedSpace, 0
            | _ -> failwith "Unexpected data"
        )
        |> Seq.indexed
        |> Seq.map (fun (i, (usedSpace, freeSpace)) -> 
            [|
                Array.init usedSpace (fun _ -> string i)
                Array.init freeSpace (fun _ -> ".")
            |]
        )
        |> Seq.collect (Seq.collect id)
        |> Seq.toArray

    let rec loop endIndex =
        match lastIndexOfAny endIndex (fun x -> x <> ".") asArray with
        | Some lastIndex ->
            let num = asArray[lastIndex]
            let startIndex = asArray |> Array.findIndex (fun x -> x = num)
            let length = lastIndex - startIndex + 1
            match findOpenSpot asArray length with
            | Some openSpot when openSpot < startIndex -> 
                swapIn asArray startIndex (length - 1) openSpot
                loop (lastIndex - 1)
            | _ -> 
                loop (startIndex - 1)
        | None ->
            ()

    loop (asArray.Length - 1)
    asArray

fileLine
|> calc
|> checksum