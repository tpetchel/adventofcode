// https://adventofcode.com/2021/day/7

// --- Day 7: The Treachery of Whales ---

// --- Part One ---

let optimizeCost input moveCost = 
    let min = input |> Array.min
    let max = input |> Array.max
    let sums = seq { min .. max } 
            |> Seq.map(fun n -> input |> Array.sumBy(fun m -> moveCost n m))
    sums |> Seq.min

let linearMoveCost = (fun a b -> b - a |> abs)

// Verify from sample data
let sampleInput = [|16;1;2;0;4;2;7;1;2;14|]
let optimalPosition = optimizeCost sampleInput linearMoveCost
printfn "%i" optimalPosition 

// Verify from puzzle data
let puzzleInput = System.IO.File.ReadAllText("input.txt").Split(',') |> Array.map (fun s -> s |> int)
let optimalPosition' = optimizeCost puzzleInput linearMoveCost
printfn "%i" optimalPosition'

// --- Part Two ---

// In this part, use a gradient move cost
let gradientMoveCost = (fun a b -> 
    let dist = b - a |> abs
    seq { 1 .. dist } |> Seq.sum)

// Verify from sample data
let optimalPosition'' = optimizeCost sampleInput gradientMoveCost
printfn "%i" optimalPosition''

// Verify from puzzle data
let optimalPosition''' = optimizeCost puzzleInput gradientMoveCost
printfn "%i" optimalPosition'''