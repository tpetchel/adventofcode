// https://adventofcode.com/2021/day/13
open System.Text.RegularExpressions

// --- Day 13: Transparent Origami ---

// --- Part One ---

let readInput filename = 
    let lines = System.IO.File.ReadLines(filename)

    let coordinateLines = lines |> Seq.takeWhile(fun line -> line <> "")
    let instructionLines = lines |> Seq.skip(1 + (coordinateLines |> Seq.length))

    let coordinates = coordinateLines |> Seq.map(fun line ->
        let arr = line.Split(',')
        (int arr[0], int arr[1])) |> Seq.toArray

    let pattern = "fold along ([x|y])=(\d+)"
    let instructions = instructionLines |> Seq.map(fun line ->
        let matched = Regex.Match(line, pattern)
        let axis  = char matched.Groups.[1].Value
        let point = int matched.Groups.[2].Value
        (axis, point))

    (coordinates, instructions)

let maxCoordinates coordinates = 
    let maxX = coordinates |> Seq.map(fun (x, _) -> x) |> Seq.max
    let maxY = coordinates |> Seq.map(fun (_, y) -> y) |> Seq.max
    (1 + maxX, 1 + maxY)

let minCoordinates coordinates = 
    let minX = coordinates |> Seq.map(fun (x, _) -> x) |> Seq.min
    let minY = coordinates |> Seq.map(fun (_, y) -> y) |> Seq.min
    (1 + minX, 1 + minY)

let foldInstructions coordinates axis line = 
    let foldVertical coordinates line = 
        let (left, right) = coordinates |> Array.partition(fun (x, _) -> x < line)
        let right' = right |> Array.map(fun (x, y) -> (2 * line - x, y))
        (left, right') ||> Array.append |> Array.distinct

    let foldHoizontal coordinates line = 
        let (top, bottom) = coordinates |> Array.partition(fun (_, y) -> y < line)
        let bottom' = bottom |> Array.map(fun (x, y) -> (x, 2 * line - y))
        (top, bottom') ||> Array.append |> Array.distinct

    match axis with 
    | 'x' -> foldVertical coordinates line
    | 'y' -> foldHoizontal coordinates line
    | _ -> failwith $"Unexpected axis '{axis}'"

let printCoordinates coordinates = 
    let (maxX, maxY) = maxCoordinates coordinates
    for i in 0 .. (maxY-1) do
        for j in 0 .. (maxX-1) do
            if coordinates |> Seq.contains (j,i) then printf "#"
            else printf "."
        printfn ""

// Validate sample data
let (sampleCoordinates, sampleInstructions) = readInput "sample.txt"
sampleCoordinates |> Array.iter(fun c -> printfn "%A" c)
sampleInstructions |> Seq.iter(fun c -> printfn "%A" c)

let final = sampleInstructions |> Seq.take 1 |> Seq.fold(fun coordinates (axis, point) -> 
    foldInstructions coordinates axis point) sampleCoordinates
printCoordinates final
printfn "%i" (final |> Array.length)

// Validate with puzzle data
let (coordinates, instructions) = readInput "input.txt"
let final' = instructions |> Seq.take 1 |> Seq.fold(fun coordinates (axis, point) -> 
    foldInstructions coordinates axis point) coordinates
printfn "%i" (final' |> Array.length)

// --- Part Two ---

let final'' = instructions |> Seq.fold(fun coordinates (axis, point) -> 
    foldInstructions coordinates axis point) coordinates
printCoordinates final''

// Final print statement makes this:

// #....###..####...##.###....##.####.#..#
// #....#..#.#.......#.#..#....#.#....#..#
// #....#..#.###.....#.###.....#.###..####
// #....###..#.......#.#..#....#.#....#..#
// #....#.#..#....#..#.#..#.#..#.#....#..#
// ####.#..#.#.....##..###...##..####.#..#

// Or LRFJBJEH
// I'm guessing they just want a human to read this and submit as the answer.