// https://adventofcode.com/2021/day/23

// --- Day 23: Amphipod ---

type Type =
    | Amber  = 0
    | Bronze = 1
    | Copper = 2
    | Desert = 3

type State =
    | WaitingToLeave = 0
    | WaitingToFinish = 3
    | Finished = 4

type Amphipod(atype, state) =  
    member this.Type = atype
    member this.State = state
    member this.Cost = 
        match this.Type with
        | Type.Amber  -> 1
        | Type.Bronze -> 10
        | Type.Copper -> 100
        | Type.Desert -> 1000
        | _ -> failwith "Invalid type"
    member this.GoalColumn =
        match this.Type with
        | Type.Amber  -> 2
        | Type.Bronze -> 4
        | Type.Copper -> 6
        | Type.Desert -> 8
        | _ -> failwith "Invalid type"

let printGrid (grid: Option<Amphipod>[,]) totalEnergy =
    let printA (a:Option<Amphipod>) =
        match a with
        | None -> '.'
        | Some(a) ->
            match a.Type with
            | Type.Amber -> 'A'
            | Type.Bronze -> 'B'
            | Type.Copper -> 'C'
            | Type.Desert -> 'D'
            | _ -> 'X'
   
    //System.Console.Clear()
    printfn ""
    printfn "#############"

    printf "#"
    for i in 0 .. (Array2D.length2 grid - 1) do
        printf "%c" (printA grid[0,i])
    printfn "#"

    printfn "###%c#%c#%c#%c###" (printA grid[1,2]) (printA grid[1,4]) (printA grid[1,6]) (printA grid[1,8]) 
    printfn "  #%c#%c#%c#%c#  " (printA grid[2,2]) (printA grid[2,4]) (printA grid[2,6]) (printA grid[2,8])
    printfn "  #########"
    printfn "Total energy = %i" totalEnergy
    printfn ""

let getAmphipods (grid: Option<Amphipod>[,]) = 
    let rows = grid |> Array2D.length1
    let cols = grid |> Array2D.length2
    seq {
        for row in 0 .. (rows - 1) do
            for col in 0 .. (cols - 1) do
                match grid[row,col] with
                | None -> ()
                | Some(a) -> yield (a, row, col)
    } |> Seq.toArray

let enumerateMoves (fromRow: int) (fromCol: int) (amphipod: Amphipod) (grid: Option<Amphipod>[,]) totalCost =
    let inline dist fromIndex toIndex = abs (toIndex - fromIndex)
    let step fromIndex toIndex =
        if toIndex = fromIndex then 0
        elif toIndex > fromIndex then 1
        else -1
    let canMoveTo toRow toCol =
        let stepRow = step fromRow toRow
        let stepCol = step fromCol toCol

        let targetCol =
            match fromRow with
            | 0 -> toCol
            | _ -> fromCol

        let indices = seq {
            if stepRow <> 0 then
                for row in (fromRow + stepRow) .. stepRow .. toRow do
                    yield (row, targetCol)
            if stepCol <> 0 then
                for col in (fromCol + stepCol) .. stepCol .. toCol do
                    yield (0, col)
        }
        indices |> Seq.forall (fun (row, col) -> grid[row,col] = None)
    let moveTo toRow toCol newState =
        // copy grid
        let grid' = Array2D.copy grid
        // update state
        let amphipod' = new Amphipod(amphipod.Type, newState)
        // move amphipod
        grid'[fromRow,fromCol] <- None
        assert (grid'[toRow,toCol] = None)
        grid'[toRow,toCol] <- Some(amphipod')
        // compute cost to move
        let cost = ((dist fromRow toRow) * amphipod'.Cost) + 
                   ((dist fromCol toCol) * amphipod'.Cost)
        
        //printf "After:"
        //printGrid grid' (totalCost + cost)

        assert ((getAmphipods grid' |> Array.length) = 8)

        (grid', totalCost + cost)
    let tryToLeave () = 
        //if toCol > 10 || toCol < 0 || toCol = 2 || toCol = 4 || toCol = 6 || toCol = 8 then false
        seq {
            for col in seq { 0;1;3;5;7;9;10 } do
                if canMoveTo 0 col then yield moveTo 0 col State.WaitingToFinish
        }
    let tryToFinish () = 
        seq {
            // is goal room occupied by another type?
            let a1 = grid[1,amphipod.GoalColumn]
            let a2 = grid[2,amphipod.GoalColumn]
            if a1 = None && (a2 = None || a2.Value.Type = amphipod.Type) then // TODO: combiine with below 
                if canMoveTo 2 amphipod.GoalColumn then 
                    yield moveTo 2 amphipod.GoalColumn State.Finished
                elif canMoveTo 1 amphipod.GoalColumn then 
                    yield moveTo 1 amphipod.GoalColumn State.Finished
        }
    match amphipod.State with
    | State.WaitingToLeave -> tryToLeave ()
    | State.WaitingToFinish -> tryToFinish ()
    | State.Finished -> Seq.empty //seq { (grid, totalCost) }
    | _ -> failwith "Invalid state"

let isSolution (grid: Option<Amphipod>[,]) =
    getAmphipods grid |> Array.forall (fun (a, _, col) -> a.GoalColumn = col)
    // let isType i j atype = 
    //     match grid[i,j] with
    //     | None -> false
    //     | Some(a) -> a.Type = atype
    // isType 1 2 Type.Amber  && isType 2 2 Type.Amber  &&
    // isType 1 4 Type.Bronze && isType 2 4 Type.Bronze &&
    // isType 1 6 Type.Copper && isType 2 6 Type.Copper &&
    // isType 1 8 Type.Desert && isType 2 8 Type.Desert

let solve grid =
    let step (grid: Option<Amphipod>[,]) totalEnergy lowestCostFound =
        //printf "Before:"
        //printGrid grid totalEnergy
        let rows = grid |> Array2D.length1
        let cols = grid |> Array2D.length2
        seq {
            for row in 0 .. (rows - 1) do
                for col in 0 .. (cols - 1) do
                match grid[row,col] with
                | None -> ()
                | Some(a) ->
                    // Enumerate all moves this amphipod can make
                    yield! enumerateMoves row col a grid totalEnergy
                        |> Seq.where (fun (_, cost) -> cost < lowestCostFound)
        } |> Seq.toList

    let mutable lowestCostFound = System.Int32.MaxValue
    let mutable queue = step grid 0 lowestCostFound
    while not (queue |> List.isEmpty) do
        match queue with
        | (grid, cost) :: tail -> 
            if cost < lowestCostFound then
                if isSolution grid then
                    printfn "Found a solution with cost %i!" cost
                    printGrid grid cost
                    lowestCostFound <- cost
                else
                    queue <- List.append (step grid cost lowestCostFound) tail
            else
                queue <- tail
        | [] -> ()
    lowestCostFound

let createSampleGrid unfolded =
    // #############
    // #...........#
    // ###B#C#B#D###
    //   #A#D#C#A#
    //   #########

    let initialState = State.WaitingToLeave

    if unfolded then // part 2
        let grid = Array2D.create 5 11 None

        grid[1,2] <- Some(new Amphipod(Type.Bronze, initialState))
        grid[2,2] <- Some(new Amphipod(Type.Desert, initialState))
        grid[3,2] <- Some(new Amphipod(Type.Desert, initialState))
        grid[4,2] <- Some(new Amphipod(Type.Amber, State.Finished))

        grid[1,4] <- Some(new Amphipod(Type.Copper, initialState))
        grid[2,4] <- Some(new Amphipod(Type.Copper, initialState))
        grid[3,4] <- Some(new Amphipod(Type.Bronze, initialState))
        grid[4,4] <- Some(new Amphipod(Type.Desert, initialState))

        grid[1,6] <- Some(new Amphipod(Type.Bronze, initialState))
        grid[2,6] <- Some(new Amphipod(Type.Bronze, initialState))
        grid[3,6] <- Some(new Amphipod(Type.Amber, initialState))
        grid[4,6] <- Some(new Amphipod(Type.Copper, State.Finished))

        grid[1,8] <- Some(new Amphipod(Type.Desert, initialState))
        grid[2,8] <- Some(new Amphipod(Type.Amber, initialState))
        grid[3,8] <- Some(new Amphipod(Type.Copper, initialState))
        grid[4,8] <- Some(new Amphipod(Type.Amber, initialState))

        grid
    else // part 1
        let grid = Array2D.create 3 11 None

        grid[1,2] <- Some(new Amphipod(Type.Bronze, initialState))
        grid[2,2] <- Some(new Amphipod(Type.Amber, State.Finished))

        grid[1,4] <- Some(new Amphipod(Type.Copper, initialState))
        grid[2,4] <- Some(new Amphipod(Type.Desert, initialState))

        grid[1,6] <- Some(new Amphipod(Type.Bronze, initialState))
        grid[2,6] <- Some(new Amphipod(Type.Copper, State.Finished))

        grid[1,8] <- Some(new Amphipod(Type.Desert, initialState))
        grid[2,8] <- Some(new Amphipod(Type.Amber, initialState))

        grid

let createPuzzleGrid unfolded =
    // #############
    // #...........#
    // ###D#D#A#A###
    //   #C#C#B#B#
    //   #########

    let initialState = State.WaitingToLeave

    if unfolded then // part 2
        let grid = Array2D.create 5 11 None

        grid[1,2] <- Some(new Amphipod(Type.Desert, initialState))
        grid[2,2] <- Some(new Amphipod(Type.Desert, initialState))
        grid[3,2] <- Some(new Amphipod(Type.Desert, initialState))
        grid[4,2] <- Some(new Amphipod(Type.Copper, initialState))

        grid[1,4] <- Some(new Amphipod(Type.Desert, initialState))
        grid[2,4] <- Some(new Amphipod(Type.Copper, initialState))
        grid[3,4] <- Some(new Amphipod(Type.Bronze, initialState))
        grid[4,4] <- Some(new Amphipod(Type.Copper, initialState))

        grid[1,6] <- Some(new Amphipod(Type.Amber, initialState))
        grid[2,6] <- Some(new Amphipod(Type.Bronze, initialState))
        grid[3,6] <- Some(new Amphipod(Type.Amber, initialState))
        grid[4,6] <- Some(new Amphipod(Type.Bronze, initialState))

        grid[1,8] <- Some(new Amphipod(Type.Amber, initialState))
        grid[2,8] <- Some(new Amphipod(Type.Amber, initialState))
        grid[3,8] <- Some(new Amphipod(Type.Copper, initialState))
        grid[4,8] <- Some(new Amphipod(Type.Bronze, initialState))

        grid
    else
        let grid = Array2D.create 3 11 None

        grid[1,2] <- Some(new Amphipod(Type.Desert, initialState))
        grid[2,2] <- Some(new Amphipod(Type.Copper, initialState))

        grid[1,4] <- Some(new Amphipod(Type.Desert, initialState))
        grid[2,4] <- Some(new Amphipod(Type.Copper, initialState))

        grid[1,6] <- Some(new Amphipod(Type.Amber, initialState))
        grid[2,6] <- Some(new Amphipod(Type.Bronze, initialState))

        grid[1,8] <- Some(new Amphipod(Type.Amber, initialState))
        grid[2,8] <- Some(new Amphipod(Type.Bronze, initialState))

        grid

// --- Part One ---

// Sample data
// let sampleCost = solve (createSampleGrid false)
// printfn "%i" sampleCost // 12521

// Puzzle data
// let cost = solve (createPuzzleGrid false)
// printfn "%i" cost // 16489

// --- Part Two ---

// Sample data
let sampleCost' = solve (createSampleGrid true)
printfn "%i" sampleCost' // 44169

// Puzzle data
let cost' = solve (createPuzzleGrid true)
printfn "%i" cost' // ???

