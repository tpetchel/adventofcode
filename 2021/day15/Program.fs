// https://adventofcode.com/2021/day/15
open System

// --- Day 15: Chiton ---

let readInput filename = 
    System.IO.File.ReadAllLines(filename)
    |> Array.map(fun (line : string) -> 
        line.ToCharArray()
        |> Array.map(fun ch -> System.Int32.Parse(ch.ToString()))) 
    |> array2D

let computeScore (grid : int[,]) points =
    points |> Seq.skip 2 |> Seq.map(fun (i,j) -> grid[i,j]) |> Seq.sum

let pairs rows cols = (seq { 0 .. (rows - 1) },
                       seq { 0 .. (cols - 1) }) ||> Seq.allPairs

let anyPath (grid : int[,]) =
    let rows = grid |> Array2D.length1
    let cols = grid |> Array2D.length2

    let s1 = (seq { 0 .. 0 },
              seq { 0 .. (cols - 1) })
             ||> Seq.allPairs

    let s2 = (seq { 0 .. (rows - 1) },
              seq { 0 .. 0 })
             ||> Seq.allPairs

    Seq.append s1 s2 |> Seq.distinct |> Seq.toList

let neighbors rows cols i j = 
    // seq { (-1,-1); (0,-1); (1,-1);
    //       (-1,0); (1,0); 
    //       (-1,1); (0,1); (1,1); }
    seq { (*(0,-1); (-1,0);*) (1,0); (0,1); }
    |> Seq.map(fun (x, y) -> (i + x, j + y))
    |> Seq.where(fun (i', j') ->
        i' >= 0 && i' < cols &&
        j' >= 0 && j' < rows)

let permutePaths' (grid : int[,]) =
    seq { anyPath grid }

let findCheapestPath' (grid : int[,]) =
    let anyScore = anyPath grid |> computeScore grid
    let scoreGrid = computeScore grid
    permutePaths' grid |> Seq.fold(fun cheapestPath path ->
        let pathScore = scoreGrid path
        if pathScore < cheapestPath then pathScore
        else cheapestPath) anyScore

let findCheapestPath (grid : int[,]) =
    let rows = grid |> Array2D.length1
    let cols = grid |> Array2D.length2
    let neighborsInGrid = neighbors rows cols
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
            let neighbors = neighborsInGrid x y
            let paths = neighbors |> Seq.choose(fun pt -> 
                findCheapest pt (x', y') path' pathCost')
            if Seq.isEmpty paths then None
            else Some(paths |> Seq.minBy(fun (_, cost) -> cost))
            // let subpaths = 
            //     neighborsInGrid x y 
            //     |> Seq.map(fun pt -> findCheapest pt (x', y') path' pathCost')
            // subpaths |> Seq.minBy(fun (_, cost) -> cost)

        // Compute 
        // let currScore = computeScore grid (List.rev ((x,y) :: points))
        // if x = x' && y = y' then
        //     if currScore < smallestScore then 
        //         smallestScore <- currScore
        //     Some((x,y) :: points)
        // elif currScore > smallestScore then None
        // else 
        //     let n = neighborsInGrid x y
        //     let r = n |> Seq.choose(fun pt -> subpaths pt (x', y') (pt :: points))
        //     if r |> Seq.isEmpty then None
        //     else 
        //         let r' = r |> Seq.minBy(fun t -> computeScore grid t)
        //         Some(r')
            //n |> Seq.fold(fun paths pt -> List.append (subpaths pt (x', y') (pt :: points)) paths) []

        // if x = x' && y = y' then seq { (x,y) :: points }
        // else 
        //     let n = neighborsInGrid x y
        //     n |> Seq.fold(fun paths pt -> Seq.append paths (subpaths pt (x', y') (pt :: points))) Seq.empty

        // if x = x' && y = y' then (x,y) :: points
        // else
        //     let n = neighbors x y rows cols
        //         // |> Seq.where(fun n -> not (points |> List.contains n))
        //     //if n |> Seq.isEmpty then None
        //     //else
        //     let paths = n |> Seq.map(fun pt -> subpaths pt (x', y') (pt :: points))
        //     //if paths |> Seq.isEmpty then None
        //     //elif paths |> Seq.length = 1 then Some(paths |> Seq.exactlyOne)
        //     //else
        //     let minPath = paths |> Seq.minBy(fun path -> computeScore grid (List.rev path))
        //     minPath

            // else
            //     let (_, cheapestNeighbors) = 
            //         n |> Seq.groupBy(fun (i, j) -> grid[i,j])
            //         |> Seq.minBy(fun (v, _) -> v)
            //     // if 1 cheapest neighbor, append it tolist and recurse cheapestSubpath ()
            //     if cheapestNeighbors |> Seq.length = 1 then
            //         let pt = cheapestNeighbors |> Seq.exactlyOne
            //         cheapestSubpath pt (x', y') (pt :: points)
            //     // else if multiple neighbors, recurse on all and return shortest
            //     else
            //         let paths = 
            //             cheapestNeighbors 
            //                 |> Seq.choose(fun pt -> cheapestSubpath pt (x', y') (pt :: points))
            //         if paths |> Seq.isEmpty then None
            //         else
            //             let minPath = paths |> Seq.minBy(fun path -> computeScore grid (List.rev path))
            //             Some(minPath)
    let start = (0, 0)
    let goal = (cols - 1, rows - 1)
    // let paths = subpaths start goal [start]
    // paths |> Seq.minBy(fun path -> computeScore grid (List.rev path))
    //let paths = subpaths start goal [start]
    //paths |> Seq.minBy(fun path -> computeScore grid (List.rev path))
    let result = findCheapest start goal [start] -grid[0,0] //(computeScore grid (anyPath grid |> List.rev))//Int32.MaxValue
    match result with 
    | Some(path, _) -> path
    | None -> failwith "no path"

    // match path with
    // | Some(x) -> 
    //     x |> Seq.rev |> Seq.iter(fun p -> printfn "%A" p)
    //     computeScore grid (Seq.rev x)
    // | None -> -1

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

// --- Part One ---

// Sample input
let sampleGrid = readInput "sample.txt"
let shortestSamplePath = findCheapestPath sampleGrid
printfn "%i" (computeScore sampleGrid shortestSamplePath)
printPath sampleGrid shortestSamplePath 

// Puzzle input
let grid = readInput "input.txt"
let shortestPath = findCheapestPath grid
printfn "%i" (computeScore grid shortestPath)
// printPath grid shortestPath 

// --- Part Two ---