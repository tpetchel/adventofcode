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

let mapTemplate (template : seq<char>) =
    //let elements = template |> Seq.pairwise |> Seq.map (fun (e1, e2) -> ((e1, e2), 1UL)) |> Map.ofSeq
    let elements = template |> Seq.pairwise |> Seq.fold (fun state pair ->
        state |> Map.change pair (fun x ->
            match x with
            | Some n -> Some (n + 1UL)
            | None -> Some (1UL))) Map.empty
    let counts = Array.create 26 0UL
    template |> Seq.iter (fun ch -> 
        let idx = (int ch) - (int 'A')
        counts[idx] <- counts[idx] + 1UL)
    (elements, counts)

let applyRule' (elements : Map<(char * char), uint64>) (counts : uint64 array) (e1, e2, e3) =
    let key = (e1, e2)
    let r = elements |> Map.tryFindKey(fun k v -> k = key && v > 0UL)
    match r with
    | Some(key) ->
        let nnn = elements[key]
        let idx = (int e3) - (int 'A')
        counts[idx] <- counts[idx] + nnn
        let elements' =
                    elements |> Map.change key (fun x ->
                        match x with
                        | Some n -> Some (0UL)//(n - 1UL)
                        | None -> failwith "Unexpected")
                    |> Map.change (e1, e3) (fun x ->
                        match x with
                        | Some n -> Some (n + nnn)
                        | None -> Some (nnn))
                    |> Map.change (e3, e2) (fun x ->
                        match x with
                        | Some n -> Some (n + nnn)
                        | None -> Some (nnn))
        elements'
    | None -> elements

let applyRules' elements (counts : uint64 array) rules =
    let matches = rules |> Seq.where(fun (e1, e2, _) ->
        let key = (e1, e2)
        elements |> Map.tryFindKey(fun k v -> k = key && v > 0UL) <> None)

    matches |> Seq.fold(fun (mp : Map<(char * char), uint64>) (e1, e2, e3) -> 
        let key = (e1, e2)
        let nnn = elements[key]
        let idx = (int e3) - (int 'A')
        counts[idx] <- counts[idx] + nnn
        mp 
        |> Map.change key (fun x ->
            match x with
            | Some n -> Some (n - nnn)//0UL)//(n - 1UL)
            | None -> failwith "Unexpected")
        |> Map.change (e1, e3) (fun x ->
            match x with
            | Some n -> Some (n + nnn)
            | None -> Some (nnn))
        |> Map.change (e3, e2) (fun x ->
            match x with
            | Some n -> Some (n + nnn)
            | None -> Some (nnn))) elements

    //rules |> Seq.fold(fun state' rule -> applyRule' state' rule) state
    // map
    // let m = rules |> Seq.map(fun rule -> applyRule' elements counts rule)
    // // reduce
    // m |> Seq.reduce(fun m1 m2 ->
    //     let s1 = m1 |> Map.toSeq
    //     let s2 = m2 |> Map.toSeq
    //     (Seq.append s1 s2) |> Seq.fold(fun mp (pair, count) -> 
    //         mp |> Map.change pair (fun x ->
    //             match x with
    //             | Some n -> Some (n + count)
    //             | None -> Some (count))) Map.empty)

// let computeScore' (elements : Map<(char * char), uint64>) =
//     let counts = Array.create 26 0UL
//     elements
//         |> Map.toArray 
//         |> Array.iter(fun ((ch1, ch2), n) ->
//             let idx1 = (int ch1) - (int 'A')
//             let idx2 = (int ch2) - (int 'A')
//             counts[idx1] <- counts[idx1] + uint64 n
//             counts[idx2] <- counts[idx2] + uint64 n)
//     // get counts of most common element and least common element, then subtract them
//     let max = counts |> Array.max
//     let min = counts |> Array.where(fun n -> n > 0UL) |> Array.min
//     max - min

// let elements = mapTemplate template
// let elements' = seq { 1 .. 10 } |> Seq.fold(fun e _ -> applyRules' e rules) elements
// let score'' = computeScore' elements'
// printfn "%i" score'' // 1588

let computeScore'' (counts : uint64 array) =
    // get counts of most common element and least common element, then subtract them
    let max = counts |> Array.max
    let min = counts |> Array.where(fun n -> n > 0UL) |> Array.min
    max - min

let (elements, counts) = mapTemplate template
let final = seq { 1 .. 40 } |> Seq.fold(fun elements' _ -> applyRules' elements' counts rules) elements
let score'' = computeScore'' counts
printfn "%i" score'' // 2188189693529

let (elements', counts') = mapTemplate template'
let final' = seq { 1 .. 40 } |> Seq.fold(fun elements'' _ -> applyRules' elements'' counts' rules') elements'
let score''' = computeScore'' counts'
printfn "%i" score''' // 1588