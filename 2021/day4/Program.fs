// https://adventofcode.com/2021/day/4

// --- Day 4: Giant Squid ---
type Card = { Values: int[,]; Marked: bool[,]; }

let readNumbers (line : string) = 
    let vals = line.Split(',')
    vals |> Array.map (fun s -> s |> int)

let rec readCards (lines : seq<string>) =
    let (|EmptySeq|_|) a = if Seq.isEmpty a then Some () else None
    match lines with
    | EmptySeq -> Seq.empty
    | _ -> seq {
        let arr = lines |> Seq.skip 1 |> Seq.take 5 |> Seq.toArray
        let vals = arr |> Array.map (fun (line : string) ->
            line.Trim().Replace("  ", " ").Split(' ') |> Array.map (fun s -> s |> int))
        yield { Values = vals |> array2D; Marked = Array2D.create 5 5 false; }
        yield! readCards (lines |> Seq.skip 1 |> Seq.skip 5)
    }

let callNumber number cards =
    cards |> Seq.map (fun card ->
        card.Values |> Array2D.iteri (fun i j n ->
            if (n = number) then card.Marked[i,j] <- true)
        card)

let allSlices card =
    let rows = card.Marked |> Array2D.length1
    let cols = card.Marked |> Array2D.length2
    seq {
        yield! seq { 0 .. rows-1 } |> Seq.map (fun i -> card.Marked[i,*])
        yield! seq { 0 .. cols-1 } |> Seq.map (fun j -> card.Marked[*,j])
    }

let isWinner card =
    let found = card |> allSlices |> Seq.tryFind (fun slice -> slice |> Array.forall ((=) true))
    match found with
    | Some(card) -> true
    | None -> false

let findWinner numbers cards =
    let rec callNext numbers cards =
        match numbers with
        | nextNumber :: remainingNumbers ->
            //printfn $"Calling {nextNumber}..."
            let newcards = callNumber nextNumber cards
            let findWinner = newcards |> Seq.tryFind (fun card -> isWinner card)
            match findWinner with
            | Some(card) -> (card, nextNumber)
            | None -> callNext remainingNumbers newcards
        | [] -> failwith "Ran out of numbers!"
    callNext (Seq.toList numbers) cards

let input = System.IO.File.ReadLines("input.txt")
let numbers = readNumbers (input |> Seq.take 1 |> Seq.exactlyOne)
let cards = readCards (input |> Seq.skip 1)

//printfn "%A" numbers
//cards |> Seq.iter (fun card -> printfn "%A" card)

let (winner, finalNumber) = findWinner numbers cards
printfn "Winning board is:\n%A" winner

// Find sum of unmarked cards
let values = winner.Values |> Seq.cast<int>
let marked = winner.Marked |> Seq.cast<bool>
let combined = Seq.zip values marked
let sum = combined |> Seq.sumBy (fun (n, b) -> if (not b) then n; else 0)
printfn "%d" (sum * finalNumber)
