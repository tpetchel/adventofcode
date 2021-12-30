// https://adventofcode.com/2021/day/17
open System.Text.RegularExpressions

// --- Day 17: Trick Shot ---

type Target = { 
    X : int;
    X' : int;
    Y : int;
    Y' : int; }

type Result =
   | Hit = 0
   | Miss = 1
   | Overshot = 2

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let parseTarget target =
   match target with
     | ParseRegex "target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)$" [Integer x; Integer x'; Integer y; Integer y']
          -> { X = x; X' = x'; Y = y; Y' = y'; }
     | _ -> failwith "Invalid target format"

let launch (target : Target) =
    let applyDrag dx =
        match dx with
        | 0 -> 0
        | dx when dx < 0 -> dx + 1
        | _ -> dx - 1
    let applyGravity dy = dy - 1
    let rec step (x, y) (dx, dy) gy target =
        // printfn $"{x},{y} -> {dx},{dy}"
        if x >= target.X && x <= target.X' && 
           y >= target.Y && y <= target.Y' then 
            //printfn $"Hit: {x},{y} -> {dx},{dy} -> {gy}"
            Some(Result.Hit, gy)
        elif x > target.X' then Some(Result.Overshot, -1)
        elif y < target.Y then Some(Result.Miss, -1)
        else step (x + dx, y + dy) (applyDrag dx, applyGravity dy) (max y gy) target

    let dxs = seq { 1 .. target.X' }
    dxs |> Seq.collect (fun dx -> 
        seq {
            // TODO: Not sure when to start
            let mutable dy = -100
            // TODO: Not sure how to make more functional, but this works for now...
            let mutable continueLooping = true
            while continueLooping do
                match step (0,0) (dx, dy) 0 target with
                | Some(Result.Miss, _) -> 
                    // TODO: Not sure when to cutoff
                    // Maybe see something converge, like not making forward progress...
                    if dy > 100 then continueLooping <- false
                | Some(Result.Overshot, _) -> continueLooping <- false
                | Some(Result.Hit, gy) -> 
                    printfn $"Hit: {dx},{dy} (greatest y = {gy})"
                    yield (dx, dy, gy)
                | _ -> failwith "Unexpected result"
                dy <- dy + 1
        })

// --- Part One ---

// Sample input
let sampleTarget = parseTarget "target area: x=20..30, y=-10..-5"
printfn "%A" sampleTarget
let sampleResults = launch sampleTarget
let sampleGreatestY = sampleResults |> Seq.map (fun (_,_,y) -> y) |> Seq.max 
printfn "%i" sampleGreatestY // 45

// Puzzle input
let target = parseTarget "target area: x=281..311, y=-74..-54"
printfn "%A" target
let results = launch target
let greatestY = results |> Seq.map (fun (_,_,y) -> y) |> Seq.max 
printfn "%i" greatestY // 2701

// --- Part Two ---

// Easy!

// Sample input
printfn "%i" (sampleResults |> Seq.length) // 112
// Puzzle input
printfn "%i" (results |> Seq.length) // 1070
