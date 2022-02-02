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

let partOne () =
    let sampleOn =
        [| "on x=10..12,y=10..12,z=10..12";
        "on x=11..13,y=11..13,z=11..13";
        "off x=9..11,y=9..11,z=9..11";
        "on x=10..10,y=10..10,z=10..10"; |]
        |> Array.map parseRebootStep
        |> reboot (isInRange range)
        |> Set.count
    printfn $"{sampleOn}" // 39

    let sampleOn' = 
        System.IO.File.ReadAllLines "sample.txt"
        |> Array.map parseRebootStep
        |> reboot (isInRange range)
        |> Set.count 
    printfn $"{sampleOn'}" // 590784

    let puzzleOn' = 
        System.IO.File.ReadAllLines "input.txt"
        |> Array.map parseRebootStep
        |> reboot (isInRange range)
        |> Set.count 
    printfn $"{puzzleOn'}" // 587785

//partOne ()

// --- Part Two ---

// I really struggled with this one. I put it aside for some time,
// but still couldn't get it after a break. 
// This is the second one I needed a hint from (on Reddit.)
// It takes a posted imperative approach and adapts it to a 
// (somewhat) more functional style.

let computeSize (cuboid: Cuboid) = 
    let distance (n: int) (m: int) = (int64 m) - (int64 n)
    let (x, x') = cuboid.Rx
    let (y, y') = cuboid.Ry
    let (z, z') = cuboid.Rz
    (distance x x') * (distance y y') * (distance z z')

let getOverlap (a: Cuboid) (b: Cuboid) =
    let xa, xa' = a.Rx
    let ya, ya' = a.Ry
    let za, za' = a.Rz
    let xb, xb' = b.Rx
    let yb, yb' = b.Ry
    let zb, zb' = b.Rz

    let x  = max xa xb
    let x' = max x (min xa' xb')
    let y  = max ya yb
    let y' = max y (min ya' yb')
    let z  = max za zb
    let z' = max z (min za' zb')

    { Rx = (x,x'); Ry = (y,y'); Rz = (z,z'); }

let reboot2 (steps: RebootStep[]) = 
    let rec calculateCubesToIndex (steps: RebootStep[]) (step: RebootStep) fromIndex =
        if fromIndex >= steps.Length then 0L
        else
            let size = computeSize step.Bounds
            if size = 0L then 0L
            else
                // If set on, add to count, if off, start with count of 0.
                let mutable count = match step.State with
                                    | State.On -> size
                                    | State.Off -> 0L
                                    | _ -> failwith "Invalid state"
                let mutable i = 0
                while i < fromIndex do
                    let cube2 = steps[i]
                    let overlapBounds = getOverlap step.Bounds cube2.Bounds
                    let overlapStep = { State = cube2.State; Bounds = overlapBounds; }
                    count <- count - calculateCubesToIndex steps overlapStep i
                    i <- i + 1
                count

    // For some reason, folks talk about adding one to the second component of each 
    // cuboid. I don't yet fully understand why, but it works. ;)
    let steps' = steps |> Array.map (fun step ->
        let (x, x') = step.Bounds.Rx
        let (y, y') = step.Bounds.Ry
        let (z, z') = step.Bounds.Rz 
        { State = step.State; Bounds = { Rx = (x,x'+1); Ry = (y,y'+1); Rz = (z,z'+1); } })

    // Compute the size of each 'on' cuboid and return the sum.
    steps' 
    |> Array.mapi (fun i step -> calculateCubesToIndex steps' step i)
    |> Array.sum

// Sample data
let sampleCount = 
    System.IO.File.ReadAllLines "sample2.txt"
    |> Array.map parseRebootStep
    |> reboot2
printfn "%i" sampleCount // 2758514936282235

// Puzzle data
let count = 
    System.IO.File.ReadAllLines "input.txt"
    |> Array.map parseRebootStep
    |> reboot2
printfn "%i" count // 1167985679908143
