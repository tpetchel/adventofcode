// https://adventofcode.com/2021/day/1

// --- Part One ---
let depths = System.IO.File.ReadLines("input.txt") |> Seq.map (fun s -> s |> int) 
let count_increasing list = list |> Seq.pairwise |> Seq.map (fun (a, b) -> if b > a then 1 else 0) |> Seq.sum
let count1 = count_increasing depths
printfn "%i" count1

// --- Part Two ---
let count2 = count_increasing (depths |> Seq.windowed 3 |> Seq.map (fun window -> window |> Seq.sum))
printfn "%i" count2