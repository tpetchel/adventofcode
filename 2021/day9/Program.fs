// https://adventofcode.com/2021/day/9

// --- Day 9: Smoke Basin ---

// --- Part One ---

let readHeightmap filename = 
    let lines = System.IO.File.ReadAllLines(filename)
    lines |> Array.map(fun (line : string) -> 
        line.ToCharArray()
        |> Array.map(fun ch -> System.Int32.Parse(ch.ToString()))) 
    |> array2D

let findLowestPoints (input : int[,]) =
    let rows = input |> Array2D.length1
    let cols = input |> Array2D.length2

    let above x y = if y = 0 then 9; else input[x, y - 1]
    let below x y = if y = (cols - 1) then 9; else input[x, y + 1]
    let left x y = if x = 0 then 9; else input[x - 1, y]
    let right x y = if x = (rows - 1) then 9; else input[x + 1, y]

    let lowPoint (x, y) =
        let n = input[x, y]
        let neighbors = seq {
            above x y; below x y;
            left x y; right x y; }
        if neighbors |> Seq.forall(fun m -> n < m) then Some((x, y))
        else None

    let rowIndices = seq { for i in 1 .. rows do yield (i - 1) }
    let colIndices = seq { for j in 1 .. cols do yield (j - 1) }
    (rowIndices, colIndices) ||> Seq.allPairs |> Seq.choose lowPoint

let getValues (input : int[,]) indices =
    indices |> Seq.map(fun (x, y) -> input[x,y])

let computeRiskLevel points = points |> Seq.map((+) 1)

// Verify sample data
let sampleInput = readHeightmap "sample.txt"
let lowestSampleIndices = sampleInput |> findLowestPoints 
let lowestSamplePoints = getValues sampleInput lowestSampleIndices
printfn "%A" lowestSamplePoints // seq [1; 0; 5; 5]

// Verify puzzle data
let puzzleInput = readHeightmap "input.txt"
let lowestPuzzleIndices = puzzleInput |> findLowestPoints
let lowestPuzzlePoints = getValues puzzleInput lowestPuzzleIndices
lowestPuzzlePoints |> Seq.iter(fun p -> printf "%i;" p) // seq [1; 0; 5; 5]
printfn ""

let riskLevels = computeRiskLevel lowestPuzzlePoints
printfn "%i" (riskLevels |> Seq.sum) // 545

// --- Part Two ---

// lowestPuzzleIndices contains the (x,y) position of each lowest point.
// Use these to discover each basin.

let mapBasin (input : int[,]) (x, y) =
    let rows = input |> Array2D.length1
    let cols = input |> Array2D.length2
    let rec step (x, y) (h, v) =
        let x' = x + h
        let y' = y + v
        if x' = rows || x' < 0 ||
           y' = cols || y' < 0 ||
           input[x',y'] = 9 then Seq.empty
        else
            input[x',y'] <- 9
            seq { 
                yield Seq.singleton (x', y')
                yield! step (x', y') (1, 0)
                yield! step (x', y') (-1, 0)
                yield! step (x', y') (0, 1)
                yield! step (x', y') (0, -1) }
    step (x, y) (0, 0) |> Seq.concat

let reduceBasins basins lowestIndices = 
    let basins' = basins |> Array2D.copy
    let indices = lowestIndices |> Seq.map(fun p -> p |> mapBasin basins')
    // Sort by desc; take 3; multiply together; print result
    indices 
        |> Seq.map(fun basin -> basin |> Seq.length)
        |> Seq.sortDescending
        |> Seq.take 3 
        |> Seq.reduce(fun m n -> m * n)

// Verify sample data
printfn "%i" (reduceBasins sampleInput lowestSampleIndices) // 1134

// Then do puzzle data.
printfn "%i" (reduceBasins puzzleInput lowestPuzzleIndices) // 950600
