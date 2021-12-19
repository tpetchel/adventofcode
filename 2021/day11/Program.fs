// https://adventofcode.com/2021/day/11

// --- Day 11: Dumbo Octopus ---

// --- Part One ---

let readInput filename = 
    let lines = System.IO.File.ReadAllLines(filename)
    lines |> Array.map(fun (line : string) -> 
        line.ToCharArray()
        |> Array.map(fun ch -> System.Int32.Parse(ch.ToString()))) 
    |> array2D

let pairs rows cols = (seq { 0 .. (rows - 1) },
                       seq { 0 .. (cols - 1) }) ||> Seq.allPairs

// Resets the energy level of the octopus at (i,j) and
// marks it as having flashed this pulse.
let flash (grid : int[,]) (lastPulse : int[,]) i j pulse = 
    grid[i,j] <- 0
    lastPulse[i,j] <- pulse

// Gets the indicies of all neighbors of the given (i,j) index.
let neighbors i j rows cols = 
    seq { (-1,-1); (0,-1); (1,-1);
            (-1,0); (1,0); 
            (-1,1); (0,1); (1,1); }
    |> Seq.map(fun (x, y) -> (i + x, j + y))
    |> Seq.where(fun (i', j') ->
        i' >= 0 && i' < cols &&
        j' >= 0 && j' < rows)

let rec pulseNeighbors (grid : int[,]) (lastPulse : int[,]) i j pulse =
    let rows = grid |> Array2D.length1
    let cols = grid |> Array2D.length2

    // Get all neighbors that haven't flashed this pulse.
    let n = neighbors i j rows cols |> Seq.where(fun (i', j') -> lastPulse[i',j'] <> pulse)
    // Increase their energy levels by 1.
    n |> Seq.iter(fun (i', j') -> grid[i',j'] <- grid[i',j'] + 1)
    // If any octopus' energy level reaches the max, flash and 
    // continue pulsing neighbors.
    n |> Seq.map(fun (i', j') -> 
        if grid[i',j'] > 9 then 
            flash grid lastPulse i' j' pulse
            1 + (pulseNeighbors grid lastPulse i' j' pulse)
        else 0)
    |> Seq.sum

// Runs the simulation one step.
let step (grid : int[,]) (lastPulse : int[,]) pulse = 
    let rows = grid |> Array2D.length1
    let cols = grid |> Array2D.length2

    // Increase the energy level of each octopus by 1.
    grid |> Array2D.iteri(fun i j v -> grid[i,j] <- v + 1)

    // Any octopus with an energy level greater than 9 flashes.
    // This increases the energy level of all adjacent octopuses by 1.
    let flashes = grid |> Array2D.mapi(fun i j v ->
        if v > 9 then 
            flash grid lastPulse i j pulse
            1 + (pulseNeighbors grid lastPulse i j pulse)
        else 0)

    // Sum all flashes.
    (pairs rows cols) |> Seq.sumBy(fun (i,j) -> flashes[i,j])

let runSimulation (grid : int[,]) (lastPulse : int[,]) pulses =
    seq { 1 .. pulses } |> Seq.sumBy(fun pulse -> step grid lastPulse pulse)

// Validate sample input first
let sampleInput = readInput "sample.txt"
let rows = sampleInput |> Array2D.length1
let cols = sampleInput |> Array2D.length2
let lastPulse = Array2D.zeroCreate rows cols
let sum = runSimulation sampleInput lastPulse 100
printfn "%i" sum

// Now validate puzzle input
let puzzleInput = readInput "input.txt"
let rows' = puzzleInput |> Array2D.length1
let cols' = puzzleInput |> Array2D.length2
let lastPulse' = Array2D.zeroCreate rows' cols'
let sum' = runSimulation puzzleInput lastPulse' 100
printfn "%i" sum' // 1562

// --- Part Two ---

// Keep running the simulation until all octopi flash in unison.

// Validate sample input first
let mutable continueLooping = true
let mutable iter = 101
while continueLooping do
    let sum = step sampleInput lastPulse iter
    if sum = rows * cols then 
        printfn "%i" iter // 195
        continueLooping <- false
    else iter <- iter + 1

// Now validate puzzle input
continueLooping <- true
iter <- 101
while continueLooping do
    let sum = step puzzleInput lastPulse iter
    if sum = rows * cols then 
        printfn "%i" iter // 268
        continueLooping <- false
    else iter <- iter + 1