// https://adventofcode.com/2021/day/12

// --- Day 12: Passage Pathing ---

// --- Part One ---

let isSmallCave (cave : string) = cave.ToLower() = cave

let isStartCave cave = cave = "start"

let isEndCave cave = cave = "end"

let isEndpoint cave = isStartCave cave || isEndCave cave

let buildGraph filename = 
    let pairs = 
        System.IO.File.ReadAllLines(filename)
        |> Array.map(fun line -> 
            let arr = line.Split('-')
            (arr[0], arr[1]))
    (Map.empty, pairs) ||> Array.fold(fun map (node', node'') -> 
        map |> Map.change node' (fun opt ->
            match opt with 
            | Some value -> Some(node'' :: value)
            | None -> Some[node''])
        |> Map.change node'' (fun opt ->
            match opt with 
            | Some value -> Some(node' :: value)
            | None -> Some[node']))

let printMap map =
    map |> Map.iter(fun k v -> printfn "%s -> %A" k v)

let findAllPaths (graph : Map<string,string list>) goal stop =
    let rec findPaths' node path =
        if goal node then [node :: path]
        elif stop node path then [[]]
        else 
            graph[node] |> List.collect(fun nextNode -> findPaths' nextNode (node :: path))
    findPaths' "start" []
        |> List.where(fun path -> not (List.isEmpty path))
        |> List.map(fun path -> path |> List.rev)

let goal node = isEndCave node
let stop node path = isSmallCave node && List.contains node path

// Validate sample input
let sampleGraph = buildGraph "sample.txt"
printMap sampleGraph
printfn "==="
let paths = findAllPaths sampleGraph goal stop
paths |> List.iter(fun path -> printfn "%A" path)
printfn "%i" (paths |> List.length) // 19
printfn "==="

// Validate puzzle input
let graph = buildGraph "input.txt"
let paths' = findAllPaths graph goal stop
printfn "%i" (paths' |> List.length) // ??

// --- Part Two ---

printfn "--- Part Two ---"

let stop' node path = 
    // We can visit large caves as many times as we'd like
    if not (isSmallCave node) then false
    else
        // We can visit endpoints at most one time
        if isEndpoint node then List.contains node path
        // OK to visit the first time
        elif not (List.contains node path) then false
        // We can visit the small cave twice if we haven't already
        // visited another small cave twice
        else path
            |> List.tryFind(fun n ->
                isSmallCave n && 
                (path |> List.where(fun n' -> n = n') 
                      |> List.length > 1)) <> None

// Validate sample input
let paths'' = findAllPaths sampleGraph goal stop'
paths'' |> List.iter(fun path -> printfn "%A" path)
printfn "%i" (paths'' |> List.length) // 103
printfn "==="

// Validate puzzleinput
let paths''' = findAllPaths graph goal stop'
printfn "%i" (paths''' |> List.length) // 144603
