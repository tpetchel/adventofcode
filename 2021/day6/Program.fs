
// https://adventofcode.com/2021/day/6

// --- Day 6: Lanternfish ---

let runSimulation timers days =
    let tickOne timer = 
        match timer with
        | 0 -> (6, 1)
        | _ -> (timer - 1, 0)
    let rec tickAll timers days = 
        //printf "Day %i: %i + " days (timers |> List.length)
        if days = 0 then timers
        else
            let tickResults = timers |> List.map tickOne
            let timers' = tickResults |> List.map (fun (tick, _) -> tick)
            let childCount = tickResults |> List.map (fun (_, children) -> children) |> List.sum
            //printfn "%i" childCount
            let newTimers = List.append timers' (List.init childCount (fun _ -> 8))
            tickAll newTimers (days - 1)
    tickAll timers days

// --- Sample data ---

let sampleInput = [3; 4; 3; 1; 2]
let result = runSimulation sampleInput 80
printfn "%i" (result |> List.length)

// --- Part One ---

let inputStr = System.IO.File.ReadAllText("input.txt")
let input = inputStr.Split(',') |> Array.map (fun s -> s |> int) |> Array.toList
let result' = runSimulation input 80
printfn "%i" (result' |> List.length)

// --- Part Two ---

//
// Running the simulation 256 times isn't feasible. Instead, track the *count* of each
// lanternfish timer.
//

// Counts the number of occurrences of each number in the given array.
// For example,
// |1, 1, 1, 3, 8|
// becomes:
// |0, 3, 0, 1, 0, 0, 0, 0, 1|
let countPopulation timers = 
    let counts = Array.create 9 0UL
    timers
        |> List.groupBy(fun timer -> timer)
        |> List.iter(fun (key, timers) -> counts[key] <- timers |> List.length |> uint64)
    counts

let runSimulation' initialPopulationCounts days =
    let shiftDown (counts : uint64 array) =
        // Remember the count of all fish ready to cycle back to 6
        let spawnCount = counts[0]
        // Create a new array
        let newCounts = Array.create 9 0UL
        // "Shift" all but the first elements from the source array
        // by one to the left
        Array.blit counts 1 newCounts 0 8
        // Fish with a count of zero are ready to cycle back to 6
        // Add them to the population
        newCounts[6] <- newCounts[6] + spawnCount
        // Fish with a count of zero spawn a new fish
        // Add those to the final slot
        newCounts[8] <- spawnCount
        newCounts
    let rec tickAll counts days = 
        if days = 0 then counts
        else tickAll (shiftDown counts) (days - 1)
    tickAll initialPopulationCounts days

// Verify sample input/output
let initialPopulationCounts = countPopulation sampleInput
let result'' = runSimulation' initialPopulationCounts 256
printfn "%u" (result'' |> Array.sum) // expect 26984457539

// Run against puzzle input
let initialPopulationCounts' = countPopulation input
let result''' = runSimulation' initialPopulationCounts' 256
printfn "%u" (result''' |> Array.sum) // expect 1754597645339
