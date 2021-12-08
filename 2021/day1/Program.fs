// https://adventofcode.com/2021/day/1

// --- Day 1: Sonar Sweep ---
let depths = System.IO.File.ReadLines("input.txt") |> Seq.map (fun s -> s |> int) 
let countIncreasing list = list |> Seq.pairwise |> Seq.map (fun (a, b) -> if b > a then 1 else 0) |> Seq.sum
let count1 = countIncreasing depths
printfn "%i" count1

// --- Part Two ---
let count2 = countIncreasing (depths |> Seq.windowed 3 |> Seq.map (fun window -> window |> Seq.sum))
printfn "%i" count2