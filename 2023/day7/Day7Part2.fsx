// --- Part Two ---
// To make things a little more interesting, the Elf introduces one additional rule. Now, J cards are jokers - wildcards that can act like whatever card would make the hand the strongest type possible.

// To balance this, J cards are now the weakest individual cards, weaker even than 2. The other cards stay in the same order: A, K, Q, T, 9, 8, 7, 6, 5, 4, 3, 2, J.

// J cards can pretend to be whatever card is best for the purpose of determining hand type; for example, QJJQ2 is now considered four of a kind. However, for the purpose of breaking ties between two hands of the same type, J is always treated as J, not the card it's pretending to be: JKKK2 is weaker than QQQQ2 because J is weaker than Q.

// Now, the above example goes very differently:

// 32T3K 765
// T55J5 684
// KK677 28
// KTJJT 220
// QQQJA 483
// 32T3K is still the only one pair; it doesn't contain any jokers, so its strength doesn't increase.
// KK677 is now the only two pair, making it the second-weakest hand.
// T55J5, KTJJT, and QQQJA are now all four of a kind! T55J5 gets rank 3, QQQJA gets rank 4, and KTJJT gets rank 5.
// With the new joker rule, the total winnings in this example are 5905.

// Using the new joker rule, find the rank of every hand in your set. What are the new total winnings?

open System
open System.IO

let example() =
    [|
        "32T3K 765"
        "T55J5 684"
        "KK677 28"
        "KTJJT 220"
        "QQQJA 483"
    |]

let readInputFile() =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Day7.txt"))

let parse (lines: string seq) =
    lines
    |> Seq.map (fun line ->
        let parts = line.Split(' ')
        parts[0], int parts[1]
    )
    
let isPair (cards: string) =
    cards
    |> Seq.groupBy id
    |> Seq.filter (fun (_, group) -> Seq.length group = 2)
    |> Seq.length = 1    

let isTwoPair (cards: string) =
    cards
    |> Seq.groupBy id
    |> Seq.filter (fun (_, group) -> Seq.length group = 2)
    |> Seq.length = 2

let isThreeOfAKind (cards: string) =
    cards
    |> Seq.groupBy id
    |> Seq.filter (fun (_, group) -> Seq.length group = 3)
    |> Seq.length = 1

let isFullHouse (cards: string) =
    cards
    |> Seq.groupBy id
    |> Seq.length = 2

let isFourOfAKind (cards: string) =
    cards
    |> Seq.groupBy id
    |> Seq.filter (fun (_, group) -> Seq.length group = 4)
    |> Seq.length = 1

let isFiveOfAKind (cards: string) =
    cards
    |> Seq.groupBy id
    |> Seq.length = 1

let getHandRank (cards: string) =
    if isFiveOfAKind cards then 7
    elif isFourOfAKind cards then 6
    elif isFullHouse cards then 5
    elif isThreeOfAKind cards then 4
    elif isTwoPair cards then 3
    elif isPair cards then 2
    else 1

let getCardRank (card: char) =
    match card with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> -1
    | 'T' -> 10
    | _ -> Int32.Parse($"{card}")

let getHandRanking (cards: string) =
    match cards.ToCharArray() |> Array.map getCardRank with
    | [| a; b; c; d; e |] -> a, b, c, d, e
    | _ -> failwith "Invalid hand"

let getBestHand (cards: string) =
    if cards.Contains "J"
    then
        true,
            [| "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "T"; "Q"; "K"; "A" |]
            |> Array.rev
            |> Array.map (fun x -> cards.Replace("J", x))
            |> Array.sortByDescending getHandRank
            |> Array.head
    else
        false, cards

let go lines =
    lines
    |> parse
    |> Seq.map (fun (hand, bid) -> 
        let isWild, bestHand = getBestHand hand
        let handRank = getHandRank bestHand
        let handRanking = getHandRanking hand
        {|
            isWild = isWild
            originalHand = hand
            bestHand = bestHand
            handRank = handRank
            handRanking = handRanking
            bid = bid
        |}
    )
    |> Seq.sortBy (fun x -> x.handRank, x.handRanking)
    |> Seq.mapi (fun i x -> i, x)

readInputFile()
|> go
|> Seq.toArray
|> Seq.map (fun (i, x) -> x.bid * (i + 1))
|> Seq.sum

