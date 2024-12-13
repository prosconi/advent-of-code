// --- Part Two ---
// Fortunately, the Elves are trying to order so much fence that they qualify for a bulk discount!

// Under the bulk discount, instead of using the perimeter to calculate the price, you need to use the number of sides each region has. Each straight section of fence counts as a side, regardless of how long it is.

// Consider this example again:

// AAAA
// BBCD
// BBCC
// EEEC
// The region containing type A plants has 4 sides, as does each of the regions containing plants of type B, D, and E. However, the more complex region containing the plants of type C has 8 sides!

// Using the new method of calculating the per-region price by multiplying the region's area by its number of sides, regions A through E have prices 16, 16, 32, 4, and 12, respectively, for a total price of 80.

// The second example above (full of type X and O plants) would have a total price of 436.

// Here's a map that includes an E-shaped region full of type E plants:

// EEEEE
// EXXXX
// EEEEE
// EXXXX
// EEEEE
// The E-shaped region has an area of 17 and 12 sides for a price of 204. Including the two regions full of type X plants, this map has a total price of 236.

// This map has a total price of 368:

// AAAAAA
// AAABBA
// AAABBA
// ABBAAA
// ABBAAA
// AAAAAA
// It includes two regions full of type B plants (each with 4 sides) and a single region full of type A plants (with 4 sides on the outside and 8 more sides on the inside, a total of 12 sides). Be especially careful when counting the fence around regions like the one full of type A plants; in particular, each section of fence has an in-side and an out-side, so the fence does not connect across the middle of the region (where the two B regions touch diagonally). (The Elves would have used the Möbius Fencing Company instead, but their contract terms were too one-sided.)

// The larger example from before now has the following updated prices:

// A region of R plants with price 12 * 10 = 120.
// A region of I plants with price 4 * 4 = 16.
// A region of C plants with price 14 * 22 = 308.
// A region of F plants with price 10 * 12 = 120.
// A region of V plants with price 13 * 10 = 130.
// A region of J plants with price 11 * 12 = 132.
// A region of C plants with price 1 * 4 = 4.
// A region of E plants with price 13 * 8 = 104.
// A region of I plants with price 14 * 16 = 224.
// A region of M plants with price 5 * 6 = 30.
// A region of S plants with price 3 * 6 = 18.
// Adding these together produces its new total price of 1206.

// What is the new total price of fencing all regions on your map?

open System.IO
open System.Collections.Generic

let example1 =
    [|
        "AAAA"
        "BBCD"
        "BBCC"
        "EEEC"
    |]

let example = 
    [|
        "RRRRIICCFF"
        "RRRRIICCCF"
        "VVRRRCCFFF"
        "VVRCCCJFFF"
        "VVVVCJJCFE"
        "VVIVCCJJEE"
        "VVIIICJJEE"
        "MIIIIIJJEE"
        "MIIISIJEEE"
        "MMMISSJEEE"
    |]

let fileLines = 
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Day12.txt"))

let data =
    fileLines
    
let up (x, y) = (x, y - 1)
let down (x, y) = (x, y + 1)
let left (x, y) = (x - 1, y)
let right (x, y) = (x + 1, y)
let width = data[0].Length
let height = data.Length
let inBounds (x, y) = 
    x >= 0 && x < width
        && y >= 0 && y < height

type Bounds = 
    | InBounds of char
    | OutOfBounds of int * int
    static member get =
        function | InBounds x -> x | OutOfBounds _ -> failwith "Out of bounds"

let getValue (x, y) =
    if inBounds (x, y)
    then InBounds(data[y][x])
    else OutOfBounds (x, y)

let fill startPosition =
    let positions = Dictionary()
    let ch = startPosition |> getValue |> Bounds.get
    let directions = [up; down; left; right]
    let q = Queue()
    q.Enqueue(startPosition)
    while q.Count > 0 do
        let pos = q.Dequeue()
        match getValue pos with
        | InBounds c when c = ch ->
            match positions.ContainsKey pos with
            | false ->
                positions.Add(pos, c)
                for d in directions do
                    let newPos = d pos
                    if not <| positions.ContainsKey newPos then
                        q.Enqueue(newPos)
            | true -> ()
        | _ -> ()
    positions

type F = 
    | Vertical
    | Horizontal

type Fence =
    { F: F
      Pos: (int * int) }

let fences = ResizeArray<Dictionary<_,_>>()

let alreadyInFence pos =
    fences
    |> Seq.exists (fun fence -> fence.ContainsKey pos)

for y, row in data |> Seq.indexed do
    for x, ch in row |> Seq.indexed do
        if not <| alreadyInFence (x, y) then
            fences.Add(fill (x, y))

let calculateDiscount (perimeter: seq<Fence>) =

    let calc fn1 fn2 fn3 = 
        perimeter
        |> Seq.groupBy fn1
        |> Seq.collect (fun (k, v) -> 
            v
            |> Seq.filter fn3
            |> Seq.map fn2
            |> Seq.sort
            |> Seq.windowed 2
            |> Seq.map (fun [|a; b|] ->
                if b - a = 1 then 1 else 0 
            )
        )
        |> Seq.sum

    let xDiscount = 
        calc (fun x -> fst x.Pos) (fun x -> snd x.Pos) (fun x -> x.F = Vertical)
    let yDiscount = 
        calc (fun x -> snd x.Pos) (fun x -> fst x.Pos) (fun x -> x.F = Horizontal)
    xDiscount + yDiscount

let prices =
    [
        for fence in fences do
            let ch = fence.Values |> Seq.head
            let area = fence.Values.Count
            let perimeters = 
                fence.Keys
                |> Seq.collect (fun pos -> 
                    [
                        { F = Horizontal; Pos = up pos }
                        { F = Horizontal; Pos = down pos }
                        { F = Vertical; Pos = left pos } 
                        { F = Vertical; Pos = right pos }
                    ]
                )
                |> Seq.filter (fun x -> 
                    match getValue x.Pos with
                    | OutOfBounds _ -> true
                    | InBounds c -> c <> ch
                )
                |> Seq.toArray
                
            let perimeter = perimeters |> Seq.length
            let discount = calculateDiscount perimeters
            let price = area * (perimeter - discount)
            //yield ch, $"{area} * ({perimeter} - {discount}) = {price}"
            yield ch, price
    ]

prices 
|> Seq.sumBy snd
