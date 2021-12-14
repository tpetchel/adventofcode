// https://adventofcode.com/2021/day/5
open System.Text.RegularExpressions

// --- Day 5: Hydrothermal Venture ---
type Line = { X1 : int; Y1 : int; X2 : int; Y2 : int; }
type Point = { X : int; Y : int; }

let isAxisAligned line =
    line.X1 = line.X2 || line.Y1 = line.Y2

let createUniverse lines =
    let rows = lines |> Seq.collect(fun line -> seq { line.X1; line.X2 }) |> Seq.max
    let cols = lines |> Seq.collect(fun line -> seq { line.Y1; line.Y2 }) |> Seq.max
    Array2D.zeroCreate (rows + 1) (cols + 1)

let stepTowards (fromPoint : Point) (toPoint : Point) stepBy =
    let delta dist by =
        let mag = abs dist
        if mag = 0 then 0
        else (dist / mag) * by
    let rec step p1 p2 by points = 
        if p1 = p2 then points
        else
            let deltaX = delta (p2.X - p1.X) by
            let deltaY = delta (p2.Y - p1.Y) by
            let newPoint =
              { X = p1.X + deltaX;
                Y = p1.Y + deltaY; }
            step newPoint toPoint by (newPoint :: points)
    step fromPoint toPoint stepBy [fromPoint]

let addPoints (universe : int [,]) line =
    let p1 = { Point.X = line.X1; Y = line.Y1; }
    let p2 = { Point.X = line.X2; Y = line.Y2; }
    let points = stepTowards p1 p2 1
    points |> Seq.iter(fun p -> 
        universe[p.X,p.Y] <- universe[p.X,p.Y] + 1)

let printUniverse (universe : int [,]) =
    let rows = universe |> Array2D.length1
    let cols = universe |> Array2D.length2
    for i in 0 .. (rows-1) do
        for j in 0 .. (cols-1) do
            let n = universe[j,i] // rotate to match problem sample
            if n = 0 then printf ". "
            else printf "%i " n
        printfn ""

let countOverlappingPoints (universe : int [,]) =
    universe |> Seq.cast<int> |> Seq.where (fun n -> n > 1) |> Seq.length

// --- Sample set ---
let solveAxisAligned input =
    // Consider only horizontal or vertical lines
    let axisAligned = input |> Seq.where isAxisAligned
    // Create minimal universe that contains all lines
    let universe = createUniverse axisAligned 
    // Curry function
    let addSamplePoints = addPoints universe
    // Add points from all line segements to universe
    axisAligned |> Seq.iter (addSamplePoints)
    // Print the universe
    // printUniverse universe
    // Return count of overlapping points
    countOverlappingPoints universe

let sampleInput = seq {
    { Line.X1 = 0; Y1 = 9; X2 = 5; Y2 = 9; }
    { Line.X1 = 8; Y1 = 0; X2 = 0; Y2 = 8; }
    { Line.X1 = 9; Y1 = 4; X2 = 3; Y2 = 4; }
    { Line.X1 = 2; Y1 = 2; X2 = 2; Y2 = 1; }
    { Line.X1 = 7; Y1 = 0; X2 = 7; Y2 = 4; }
    { Line.X1 = 6; Y1 = 4; X2 = 2; Y2 = 0; }
    { Line.X1 = 0; Y1 = 9; X2 = 2; Y2 = 9; }
    { Line.X1 = 3; Y1 = 4; X2 = 1; Y2 = 4; }
    { Line.X1 = 0; Y1 = 0; X2 = 8; Y2 = 8; }
    { Line.X1 = 5; Y1 = 5; X2 = 8; Y2 = 2; }
}

// Solve and print solution
printfn $"{solveAxisAligned sampleInput}"

// --- Part One ---
let parseLine (line : string) =
    let pattern = "(\d+),(\d+) -> (\d+),(\d+)"
    let matched = Regex.Match(line, pattern)
    let x1  = int matched.Groups.[1].Value
    let y1  = int matched.Groups.[2].Value
    let x2  = int matched.Groups.[3].Value
    let y2  = int matched.Groups.[4].Value
    { Line.X1 = x1; Y1 = y1; X2 = x2; Y2 = y2; }

let input = System.IO.File.ReadLines("input.txt") |> Seq.map parseLine
printfn $"{solveAxisAligned input}"

// --- Part Two ---
let solveGeneral input =
    // Create minimal universe that contains all lines
    let universe = createUniverse input 
    // Curry function
    let addSamplePoints = addPoints universe
    // Add points from all line segements to universe
    input |> Seq.iter (addSamplePoints)
    // Print the universe
    //printUniverse universe
    // Return count of overlapping points
    countOverlappingPoints universe

printfn $"{solveGeneral sampleInput}"
printfn $"{solveGeneral input}"