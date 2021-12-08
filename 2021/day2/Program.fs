// https://adventofcode.com/2021/day/2

// --- Day 2: Dive! ---
let parseCommand (command: string) =
    let a = command.Split()
    let direction = a[0]
    let amount = a[1] |> int
    match direction with
    | "forward" -> Some(amount, 0)
    | "down" -> Some(0, amount)
    | "up" -> Some(0, -amount)
    | _ -> None

let initial = (0, 0)
let input = System.IO.File.ReadLines("input.txt")

let solver parser =
    (initial, input) ||> Seq.fold (fun (a, b) command -> 
        match parser command with
        | Some(x, y) -> (a + x, b + y)
        | None -> (a, b))

let (distance, depth) = solver parseCommand
printfn "%i" (distance * depth)

// --- Part Two ---
let parseCommand' (aim, position, depth) (command:string) =
    let a = command.Split()
    let direction = a[0]
    let amount = a[1] |> int
    match direction with
    | "forward" -> (aim, position + amount, depth + (aim * amount))
    | "down" -> (aim + amount, position, depth)
    | "up" -> (aim - amount, position, depth)
    | _ -> (aim, position, depth)

let initialState = (0, 0, 0)

let solver' parser =
    (initialState, input) ||> Seq.fold (fun accState command -> parser accState command)

let (aim, position, depth') = solver' parseCommand'
printfn "%i" (position * depth')