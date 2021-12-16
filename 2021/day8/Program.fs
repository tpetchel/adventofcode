// https://adventofcode.com/2021/day/8
open System.Text.RegularExpressions

// --- Day 8: Seven Segment Search ---

type Entry = { Signals: string array; Outputs: string array }

// --- Part One ---

let segmentCounts = Map [ 
    (0, 6); (1, 2); (2, 5); (3, 5); (4, 4);
    (5, 5); (6, 6); (7, 3); (8, 7); (9, 6) ]

let parseEntries file = 
    System.IO.File.ReadAllLines(file) 
    |> Array.map(fun line -> 
        let pattern = "(.+)\|(.+)"
        let matched = Regex.Match(line, pattern)
        { Signals = matched.Groups[1].Value.Trim().Split(' ');
          Outputs = matched.Groups[2].Value.Trim().Split(' '); })

let countDigits (values : string array) length =
    values |> Array.where(fun value -> value.Length = length)

// Validate with sample data
let sampleEntries = parseEntries "sample.txt"
// printfn "%A" sampleEntries
let sampleOutputs = sampleEntries |> Array.map(fun entry -> entry.Outputs) |> Array.concat
let countEasy = countDigits sampleOutputs
let count = [|1;4;7;8|] |> Array.map(fun n -> segmentCounts[n]) |> Array.map(countEasy) |> Array.concat |> Array.length
printfn "%i" count

// Validate with puzzle data
let puzzleEntries = parseEntries "input.txt"
let puzzleOutputs = puzzleEntries |> Array.map(fun entry -> entry.Outputs) |> Array.concat
let countEasy' = countDigits puzzleOutputs
let count' = [|1;4;7;8|] |> Array.map(fun n -> segmentCounts[n]) |> Array.map(countEasy') |> Array.concat |> Array.length
printfn "%i" count'

// --- Part Two ---

let sort (s : string) = s |> Seq.sort |> System.String.Concat

let decodeSignals (signals : string array) =
    let matchLength (signals : string array) length = 
        signals |> Array.find(fun signal -> signal.Length = length)
                |> Set.ofSeq

    // Decode signals that have a unique number of segments
    let s1 = matchLength signals 2
    let s4 = matchLength signals 4
    let s7 = matchLength signals 3
    let s8 = matchLength signals 7

    //
    // Here we use a bit of deduction to figure out which signal maps
    // to which number. This smells like an exact cover problem and could likley
    // be rewritten a bit more elegantly.
    //

    // 9 is a superset of 4
    let s9 = signals
            |> Array.where(fun signal -> signal.Length = 6)
            |> Array.where(fun c -> c |> Set.ofSeq |> Set.isSubset s4)
            |> Array.exactlyOne
            |> Set.ofSeq

    // 3 is a subset of 9 that is a superset of 1
    let s3 = signals
            |> Array.where(fun signal -> signal.Length = 5)
            |> Array.where(fun c -> c |> Set.ofSeq |> Set.isSuperset s9)
            |> Array.where(fun c -> c |> Set.ofSeq |> Set.isSubset s1)
            |> Array.exactlyOne
            |> Set.ofSeq

    // 5 is a subset of 9 that isn't 3
    let s5 = signals
            |> Array.where(fun signal -> signal.Length = 5)
            |> Array.where(fun c -> c |> Set.ofSeq |> Set.isSuperset s9)
            |> Array.map(fun c -> c |> Set.ofSeq)
            |> Array.filter(fun s -> s <> s3)
            |> Array.exactlyOne

    // 2 is the remaining one of length 5
    let s2 = signals
            |> Array.where(fun signal -> signal.Length = 5)
            |> Array.map(fun c -> c |> Set.ofSeq)
            |> Array.filter(fun s -> s <> s3 && s <> s5)
            |> Array.exactlyOne

    // Two left! - 0 and 6, both of length 6
    let remaining = signals
                 |> Array.where(fun signal -> signal.Length = 6)
                 |> Array.map(fun c -> c |> Set.ofSeq)
                 |> Array.filter(fun s -> s <> s9)

    // 0 intersects with 1
    let s0 = remaining
          |> Array.where(fun c -> c |> Set.isSubset s1)
          |> Array.exactlyOne

    // 6 is the remaining set
    let s6 = remaining
          |> Array.where(fun c -> c <> s0)
          |> Array.exactlyOne

    let keys = seq { s0; s1; s2; s3; s4; s5; s6; s7; s8; s9 }
            |> Seq.map(fun s -> s |> Set.toArray |> System.String.Concat |> sort)
    let vals = seq { 0 .. 9 } |> Seq.map string
    Seq.zip keys vals |> Map

let decodeEntry (entry : Entry) = 
    let toInt (strings : string array) = String.concat "" strings |> int
    let patterns = decodeSignals entry.Signals
    entry.Outputs |> Array.map(fun output -> patterns[output |> sort]) |> toInt

// Validate with sample data
let decodeEntries (entries : Entry array) = 
    entries |> Array.map decodeEntry |> Array.sum

let sum = decodeEntries sampleEntries
printfn "%i" sum // 61229

// Validate with puzzle data
let sum' = decodeEntries puzzleEntries
printfn "%i" sum' // 936117