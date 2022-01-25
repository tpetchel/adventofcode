// https://adventofcode.com/2021/day/22
open System.Text.RegularExpressions

// --- Day 22: Reactor Reboot ---

type State =
    | On = 0
    | Off = 1

type Cuboid = { 
    Rx: (int * int);
    Ry: (int * int);
    Rz: (int * int); }

type RebootStep = {
    State: State;
    Bounds: Cuboid; }

let (|State|_|) (str: string) =
   match str with
   | "on" -> Some(State.On)
   | "off" -> Some(State.Off)
   | _ -> None

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let parseRebootStep str =
    match str with
    | ParseRegex "(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)$" [State s; Integer x; Integer x'; Integer y; Integer y'; Integer z; Integer z';]
        -> { State = s;
             Bounds = { Rx = (x, x'); Ry = (y, y'); Rz = (z, z'); }; }
    | _ -> failwith "Invalid reboot step"

let makeCubes (cuboid: Cuboid) =
    let (rx, rx') = cuboid.Rx
    let (ry, ry') = cuboid.Ry
    let (rz, rz') = cuboid.Rz
    seq {
        for x = rx to rx' do
            for y = ry to ry' do
                for z = rz to rz' do
                    yield (x,y,z) }
    |> Set.ofSeq

let applyRebootStep step steps = 
    let cubes = makeCubes step.Bounds
    match step.State with
    | State.On -> steps + cubes
    | State.Off -> steps - cubes
    | _ -> invalidArg (nameof step) "Invalid reboot step"

let reboot isInRange steps = 
    steps |> Array.fold (fun steps step -> 
        if isInRange step then applyRebootStep step steps
        else steps) Set.empty

let printStep step =
    match step.State with
    | State.On -> printf "on "
    | State.Off -> printf "off "
    | _ -> printf "ERROR "
    let cuboid = step.Bounds
    let (rx, rx') = cuboid.Rx
    let (ry, ry') = cuboid.Ry
    let (rz, rz') = cuboid.Rz
    printfn $"x={rx}..{rx'},y={ry}..{ry'},z={rz}..{rz'}"

let printSteps steps = steps |> Array.iter printStep

// --- Part One ---

let isInRange range step = 
    let (rx, rx') = range.Rx
    let (ry, ry') = range.Ry
    let (rz, rz') = range.Rz
    let (x, x') = step.Bounds.Rx
    let (y, y') = step.Bounds.Ry
    let (z, z') = step.Bounds.Rz
    x >= rx && x' <= rx' && y >= ry && y' <= ry' && z >= rz && z' <= rz'

let range = { Rx = (-50, 50); Ry = (-50, 50); Rz = (-50, 50); }

// let sampleOn =
//     [| "on x=10..12,y=10..12,z=10..12";
//        "on x=11..13,y=11..13,z=11..13";
//        "off x=9..11,y=9..11,z=9..11";
//        "on x=10..10,y=10..10,z=10..10"; |]
//     |> Array.map parseRebootCommand
//     |> reboot (isInRange range)
//     |> Set.count
// printfn $"{sampleOn}" // 39

// let sampleOn' = 
//     System.IO.File.ReadAllLines "sample.txt"
//     |> Array.map parseRebootCommand
//     |> reboot (isInRange range)
//     |> Set.count 
// printfn $"{sampleOn'}" // 590784

// let puzzleOn' = command
//     System.IO.File.ReadAllLines "input.txt"
//     |> Array.map parseRebootCommand
//     |> reboot (isInRange range)
//     |> Set.count 
// printfn $"{puzzleOn'}" // 587785

// --- Part Two ---

// let reduceSteps (steps: RebootStep[]) =
//     let rec reduce steps =
//         let isSubstep step = steps |> Array.tryFind (fun s -> isInRange s.Bounds step)
//         let steps' = Array.choose (fun step -> isSubstep step) steps
//         if (steps.Length <> steps'.Length) then reduce steps'
//         else steps'
//     reduce steps

(*
Here's another one that threw me. My approach was good for part 1, but 
fell flat in part 2 due to the number of elements being considered.

Revised approach:
read input
map each to command
partition into on and off cuboid sets
foreach off cuboid
    find all on cuboids that overlap with the off cuboid
    for each on cuboid that overlaps
        replace the on cuboid with the set of sub-cubiods that don't overlap
answer <- count elements in remaining set of on cuboids
*)

let computeSize cubiod = 
    let distance (n: int) (m: int) = 
        uint64 (abs ((int64 n) - (int64 m)))
    let (x, x') = cubiod.Rx
    let (y, y') = cubiod.Ry
    let (z, z') = cubiod.Rz
    (distance x x') * (distance y y') * (distance z z')

let sampleSteps = 
    System.IO.File.ReadAllLines "sample2.txt"
    |> Array.map parseRebootStep

// partition into on and off cuboid sets
let onSteps, offSteps = 
    sampleSteps |> Array.partition (fun step -> step.State = State.On)

(*
foreach off cuboid
    find all on cuboids that overlap with the off cuboid
    for each on cuboid that overlaps
        replace the on cuboid with the set of sub-cubiods that don't overlap
answer <- count elements in remaining set of on cuboids
*)

// let sample2On = 
//     System.IO.File.ReadAllLines "sample2.txt"
//     |> Array.map parseRebootCommand

// sample2On |> printSteps
// printfn "==="
// printfn $"{sample2On.Length}"
// let sample2On' = reduceSteps sample2On
// printfn $"{sample2On'.Length}"

//printfn $"{sample2On}" // 
