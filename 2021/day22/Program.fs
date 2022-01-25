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

//partOne

// --- Part Two ---

(*
Here's another one that threw me. My approach was good for part 1, but fell flat in part 2 due to the number of elements being considered.
I took some time off from this, because life. Let's try it again now.

Revised approach:
read input; map each line of input to a command
partition into "on" and "off" cuboid sets
countOn <- compute sum of all "on" cuboids
foreach off cuboid, off
    countOn <- countOn - (overlap(off allOn) + overlap(off allOff)
    allOff <- allOff :: off
*)

let computeSize cubiod = 
    let distance (n: int) (m: int) = 
        uint64 (abs ((int64 n) - (int64 m)))
    let (x, x') = cubiod.Rx
    let (y, y') = cubiod.Ry
    let (z, z') = cubiod.Rz
    (distance x x') * (distance y y') * (distance z z')

let partition steps = 
  steps |> Array.partition (fun step -> step.State = State.On)

// Computes the number of points the two steps overlap
let overlap (step: RebootStep) (steps: seq<RebootStep>) =
    let intersection (n1, n1') (n2, n2') =
        if (n1 <= n2 && n1' >= n2') ||
           (n2 <= n1 && n2' >= n1') ||
           (n1 <= n2 && n1' >= n2) ||
           (n1 <= n2' && n1' >= n2') then 
            Some(max n1 n2, min n1' n2')
        else 
            assert ((n1 < n2 && n1' < n2') || (n1 > n2 && n1' > n2'))
            None
    let bounds1 = step.Bounds
    steps |> Seq.sumBy (fun step2 -> 
        let bounds2 = step2.Bounds
        match intersection bounds1.Rx bounds2.Rx with
        | Some(x, x') -> 
            match intersection bounds1.Ry bounds2.Ry with
            | Some(y, y') -> 
                match intersection bounds1.Rz bounds2.Rz with
                | Some(z, z') -> computeSize { Rx = (x, x'); Ry = (y, y'); Rz = (z, z'); } 
                | None -> 0UL
            | None -> 0UL
        | None -> 0UL)

let reboot2 steps = 
    // partition into on and off cuboid sets
    let onSteps, offSteps = partition steps

    // compute sum of all "on" cuboids
    let initialOnCount = onSteps |> Array.sumBy (fun step -> computeSize step.Bounds)

    let (finalOnCount, _) =
        offSteps |> Array.fold (fun (onCount, offSteps) offStep -> 
            (onCount - (overlap offStep onSteps) + (overlap offStep offSteps), (offStep :: offSteps))) (initialOnCount, List.empty)
    finalOnCount

let sampleOn = 
    System.IO.File.ReadAllLines "sample.txt"
    |> Array.map parseRebootStep
    |> reboot2

printfn "%i" sampleOn
//t: 2758514936282235
//1: 2441245014278670
