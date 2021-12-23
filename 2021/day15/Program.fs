// https://adventofcode.com/2021/day/15
open System

// --- Day 15: Chiton ---

let readInput filename = 
    System.IO.File.ReadAllLines(filename)
    |> Array.map(fun (line : string) -> 
        line.ToCharArray()
        |> Array.map(fun ch -> System.Int32.Parse(ch.ToString()))) 
    |> array2D

let computeCost (grid : int[,]) points =
    points |> Seq.map(fun (i,j) -> grid[i,j]) |> Seq.sum

let unvisited (vis : bool[,]) = 
    let rows = vis |> Array2D.length1
    let cols = vis |> Array2D.length2
    seq {
        for i in 0 .. rows-1 do
            for j in 0 .. cols-1 do
                if not vis[i,j] then
                    yield (i,j); }

let findCheapestPath (grid : int[,]) =
    let rows = grid |> Array2D.length1
    let cols = grid |> Array2D.length2

    let neighbors i j = 
        seq { (1,0); (0,1); }
        |> Seq.map(fun (x, y) -> (i + x, j + y))
        |> Seq.where(fun (i', j') ->
            i' >= 0 && i' < cols &&
            j' >= 0 && j' < rows)

    let mutable cheapest = Int32.MaxValue
    let rec findCheapest (x,y) (x',y') path pathCost =
        // Add ourself to the path
        let path' = (x,y) :: path
        // Update cost
        let pathCost' = pathCost + grid[x,y]

        // Base case: we've reached the end
        if x = x' && y = y' then 
            //printfn "Found a solution with cost %i" pathCost'
            if pathCost' < cheapest then
                cheapest <- pathCost'
            Some(path', pathCost')
        // Base case: too expensive
        elif pathCost' > cheapest then None
        // Recursive case: compute all subpaths and choose the shortest
        else
            let paths = neighbors x y |> Seq.choose(fun pt -> 
                findCheapest pt (x', y') path' pathCost')
            if Seq.isEmpty paths then None
            else Some(paths |> Seq.minBy(fun (_, cost) -> cost))

    let start = (0, 0)
    let goal = (cols - 1, rows - 1)
    let result = findCheapest start goal [start] -grid[0,0] 
    match result with 
    | Some(path, _) -> path
    | None -> failwith "no path"

let printPath (grid : int[,]) path =
    let consoleColor (fc : ConsoleColor) = 
        let current = Console.ForegroundColor
        Console.ForegroundColor <- fc
        { new IDisposable with
            member x.Dispose() = Console.ForegroundColor <- current }

    let rows = grid |> Array2D.length1
    let cols = grid |> Array2D.length2
    for i in 0 .. (rows - 1) do
        for j in 0 .. (cols - 1) do
            if path |> Seq.contains((i,j)) then 
                use _ = consoleColor ConsoleColor.Red in printf "%i" grid[i,j]
            else 
                printf "%i" grid[i,j]
        printfn ""

// findCheapestPath worked OK for the sample input, but not for the puzzle input
// (I let it run for hours!)
// I knew that Dijkstra’s algorithm might help.
// Adapted from:
// https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
// https://medium.com/omarelgabrys-blog/path-finding-algorithms-f65a8902eb40
// https://levelup.gitconnected.com/dijkstras-shortest-path-algorithm-in-a-grid-eb505eb3a290
// This could surely be made more 'functional', but just getting something working for now.

let dijkstra (grid : int[,]) start goal =
    let rows = grid |> Array2D.length1
    let cols = grid |> Array2D.length2

    let neighbors i j = 
        seq { (-1,0); (0,-1); (1,0); (0,1); }
        |> Seq.map(fun (x, y) -> (i + x, j + y))
        |> Seq.where(fun (i', j') ->
            i' >= 0 && i' < cols &&
            j' >= 0 && j' < rows)

    let cost = Array2D.init rows cols (fun i j -> grid[i,j])
    cost[fst start, snd start] <- 0

    let tcost = Array2D.create rows cols Int32.MaxValue
    tcost[fst start, snd start] <- 0

    let vis = Array2D.create rows cols false

    let mutable curr = start
    let gi = fst goal
    let gj = snd goal
    while not vis[gi,gj] do
        let ci = fst curr
        let cj = snd curr
        neighbors ci cj
            |> Seq.iter (fun (ni,nj) -> 
                if not vis[ni,nj] then
                    let t = tcost[ci,cj] + cost[ni,nj]
                    if t < tcost[ni,nj] then tcost[ni,nj] <- t)
        vis[ci,cj] <- true
        if not vis[gi, gj] then
            curr <- unvisited vis |> Seq.minBy (fun (i,j) -> tcost[i,j])

    tcost[gi, gj]

// --- Part One ---

printfn "--- Part One ---"

let start = (0, 0)
let goal (grid : 'a[,]) =
    let rows = grid |> Array2D.length1
    let cols = grid |> Array2D.length2
    (rows - 1, cols - 1)

// Sample input - naive implementation
let sampleGrid = readInput "sample.txt"
let shortestSamplePath = findCheapestPath sampleGrid
printfn "Naive, sample input: %i" (computeCost sampleGrid (shortestSamplePath |> List.skip 2))
printPath sampleGrid shortestSamplePath
printfn "==="

// Sample input - Dijkstra's algorithm
let shortestSamplePathCost' = dijkstra sampleGrid start (goal sampleGrid)
printfn "Dijkstra, sample input: %i" shortestSamplePathCost' // 40
printfn "==="

// Puzzle input - Dijkstra's algorithm
let grid = readInput "input.txt"
let shortestPathCost = dijkstra grid start (goal grid)
printfn "Dijkstra, puzzle input: %i" shortestPathCost // 472

// --- Part Two ---

let expand (grid : int[,]) tiles =
    let makeTile n =
        let increment x n =
            let x' = x + n
            if x' > 9 then (x' - 9); else x'
        Array2D.copy grid |> Array2D.map(fun v -> increment v n)

    let rows = grid |> Array2D.length1
    let cols = grid |> Array2D.length2
    let rows' = rows * tiles
    let cols' = cols * tiles

    let grid' = Array2D.zeroCreate rows' cols'
    for i in 0 .. tiles - 1 do
        for j in 0 .. tiles - 1 do
            let tile = makeTile (i + j)
            Array2D.blit tile 0 0 grid' (i * rows) (j * cols) rows cols
    grid'

printfn "--- Part Two ---"

let expandBy = 5

// Sample input - Dijkstra's algorithm, expanded
let sampleGrid' = expand sampleGrid expandBy
let shortestSamplePathCost'' = dijkstra sampleGrid' start (goal sampleGrid')
printfn "Dijkstra, sample input expanded: %i" shortestSamplePathCost'' // 315
printfn "==="

// Puzzle input - Dijkstra's algorithm, expanded
let grid' = expand grid expandBy
let shortestPathCost' = dijkstra grid' start (goal grid')
printfn "Dijkstra, puzzle input expanded: %i" shortestPathCost' // 2851
