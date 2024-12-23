open System.IO

let readInputFile() =
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Day23.txt"))

let example =
    [
        "kh-tc"
        "qp-kh"
        "de-cg"
        "ka-co"
        "yn-aq"
        "qp-ub"
        "cg-tb"
        "vc-aq"
        "tb-ka"
        "wh-tc"
        "yn-cg"
        "kh-ub"
        "ta-co"
        "de-co"
        "tc-td"
        "tb-wq"
        "wh-td"
        "ta-ka"
        "td-qp"
        "aq-cg"
        "wq-ub"
        "ub-vc"
        "de-ta"
        "wq-aq"
        "wq-vc"
        "wh-yn"
        "ka-de"
        "kh-ta"
        "co-tc"
        "wh-qp"
        "tb-vc"
        "td-yn"
    ]

let lines = readInputFile()

let splitIntoTwo (delimiter: string) (line: string) =
    match line.Split(delimiter) with
    | [| left; right |] -> left, right
    | _ -> failwithf "Invalid line: %s" line

let connections =
    lines
    |> Seq.map (splitIntoTwo "-")
    |> Seq.collect (fun (a, b) -> [(a, b); (b, a)])
    |> Seq.distinct
    |> Seq.groupBy fst
    |> Seq.map (fun (k, v) -> (k, v |> Seq.map snd |> Seq.toArray))
    |> dict

let rec findAllSetsOfThree (path: Map<string,bool>) position =
    let areAllConnected() =
        path
        |> Seq.forall (fun x -> connections[x.Key] |> Seq.exists (fun y -> y = position))

    match path.ContainsKey position with
    | true -> [None]
    | false ->
        match areAllConnected() with
        | false -> [None]
        | true ->
            let newPath = Map.add position true path
            if newPath.Count = 3 then
                [Some(newPath)]
            else
                seq {
                    for n in connections[position] do
                        yield! findAllSetsOfThree newPath n
                }
                |> Seq.toList
        
connections.Keys
|> Seq.filter (fun x -> x.StartsWith "t")
|> Seq.map (findAllSetsOfThree Map.empty)
|> Seq.collect (Seq.choose id)
|> Seq.map (fun x -> x.Keys |> String.concat "-")
|> Seq.distinct
|> Seq.toArray
|> _.Length
