// https://adventofcode.com/2021/day/14
open System.Text.RegularExpressions

// --- Day 14: Extended Polymerization ---

// --- Part One ---

let readInput filename =
    let lines = System.IO.File.ReadLines(filename)

    // read polymer template
    let template = lines |> Seq.head

    // read rules
    let pattern = "([A-Z])([A-Z]) -> ([A-Z])"
    let rules = lines |> Seq.skip 2 |> Seq.map(fun line ->
        let matched = Regex.Match(line, pattern)
        let e1 = char matched.Groups.[1].Value
        let e2 = char matched.Groups.[2].Value
        let e3 = char matched.Groups.[3].Value
        (e1, e2, e3))

    (template |> seq, rules)

let matchRule (e1 : char) (e2 : char) (rules : seq<char * char * char>) =
    let m = rules |> Seq.tryFind(fun (r1, r2, _) -> e1 = r1 && e2 = r2)
    match m with
    | Some(_, _, insert) -> seq { e1; insert; e2 }
    | None -> seq { e1; e2 }

let applyRules template rules =
    // mark
    let mark = template |> Seq.pairwise |> Seq.map(fun (e1, e2) -> matchRule e1 e2 rules)
    // sweep
    mark |> Seq.reduceBack(fun a b -> Seq.append (a |> Seq.take ((Seq.length a) - 1)) b)

let computeScore (template : seq<char>) =
    // count common element and least common element, then subtract them
    let used = template |> Seq.distinct
    let counts = used |> Seq.map(fun ch -> 
        let count = template |> Seq.where(fun ch' -> ch = ch') |> Seq.length
        (ch, count))
    let (_, max) = counts |> Seq.maxBy(fun (_, count) -> count)
    let (_, min) = counts |> Seq.minBy(fun (_, count) -> count)
    max - min

// Sample input
let (template, rules) = readInput "sample.txt"
let result = seq { 1 .. 10 } |> Seq.fold(fun t _ -> applyRules t rules) template
//printfn "%s" (result |> Seq.toArray |> System.String)
let score = computeScore result
printfn "%i" score // 1588

// Puzzle input
let (template', rules') = readInput "input.txt"
let result' = seq { 1 .. 10 } |> Seq.fold(fun t _ -> applyRules t rules') template'
let score' = computeScore result'
printfn "%i" score' // 2768

// --- Part Two ---

printfn "--- Part Two ---"

// OK, got bitten again by exponential growth ;)
// It isn't feasible to do 40 iterations on sample input, let alone puzzle input.
// This time, track all unique pairs and their counts in a map.

// // Sample input
// let result'' = seq { 1 .. 40 } |> Seq.fold(fun t _ -> applyRules t rules) template
// //printfn "%s" (result |> Seq.toArray |> System.String)
// let score'' = computeScore result''
// printfn "%i" score''

let createPairs (template : seq<char>) =
    template |> Seq.pairwise |> Seq.fold (fun state pair ->
        state |> Map.change pair (fun x ->
            match x with
            | Some n -> Some (n + 1UL)
            | None -> Some (1UL))) Map.empty

let createPairCounts (template : seq<char>) =
    let counts = Array.create 26 0UL
    template |> Seq.iter (fun ch -> 
        let idx = (int ch) - (int 'A')
        counts[idx] <- counts[idx] + 1UL)
    counts

let applyRules' pairs (counts : uint64 array) rules =
    // First find all matching rules
    let matches = rules |> Seq.where(fun (e1, e2, _) ->
        let key = (e1, e2)
        pairs |> Map.tryFindKey(fun k v -> k = key && v > 0UL) <> None)

    // Apply each rule
    matches |> Seq.fold(fun (map : Map<(char * char), uint64>) (e1, e2, e3) -> 
        let key = (e1, e2)
        let pairCount = pairs[key]

        let index = (int e3) - (int 'A')
        counts[index] <- counts[index] + pairCount

        map |> Map.change key (fun x ->
                match x with
                | Some n -> Some (n - pairCount)
                | None -> failwith "Unexpected")
            |> Map.change (e1, e3) (fun x ->
                match x with
                | Some n -> Some (n + pairCount)
                | None -> Some (pairCount))
            |> Map.change (e3, e2) (fun x ->
                match x with
                | Some n -> Some (n + pairCount)
                | None -> Some (pairCount))) pairs

let computeScore' (counts : uint64 array) =
    // get counts of most common element and least common element, then subtract them
    let max = counts |> Array.max
    let min = counts |> Array.where(fun n -> n > 0UL) |> Array.min
    max - min

// Validate sample data
let pairs = createPairs template
let counts = createPairCounts template
let _ = seq { 1 .. 40 } |> Seq.fold(fun pairs' _ -> 
    applyRules' pairs' counts rules) pairs
printfn "%i" (computeScore' counts) // 2188189693529

// Validate puzzle data
let pairs' = createPairs template'
let counts' = createPairCounts template'
let _ = seq { 1 .. 40 } |> Seq.fold(fun pairs'' _ -> 
    applyRules' pairs'' counts' rules') pairs'
printfn "%i" (computeScore' counts') // 2914365137499