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

let computeSize2 cubiod offBounds = 
    let hitTest (a,b,c) =
        let result = offBounds |> Array.tryFind (fun cubiod ->
            let (x, x') = cubiod.Rx
            let (y, y') = cubiod.Ry
            let (z, z') = cubiod.Rz
            a >= x && a <= x' &&
            b >= y && b <= y' &&
            c >= z && c <= z')
        match result with
        | None -> 1UL
        | Some(_) -> 0UL
    let mutable sum = 0UL
    let (x, x') = cubiod.Rx
    let (y, y') = cubiod.Ry
    let (z, z') = cubiod.Rz
    for a in x .. x' do
        for b in y .. y' do
            for c in z .. z' do
                sum <- sum + hitTest (a,b,c)
    printfn "cube is: %i" sum
    sum

let partition steps = 
  let onSteps, offSteps = steps |> Array.partition (fun step -> step.State = State.On)
  let onBounds = onSteps |> Array.map (fun step -> step.Bounds)
  let offBounds = offSteps |> Array.map (fun step -> step.Bounds)
  (onBounds, offBounds)

let rec split (onBounds: Cuboid) (offBounds: Cuboid[]) = 
    let intersection (n1, n1') (n2, n2') =
        if (n1 <= n2 && n1' >= n2') ||
           (n2 <= n1 && n2' >= n1') ||
           (n1 <= n2 && n1' >= n2) ||
           (n1 <= n2' && n1' >= n2') then 
            Some(max n1 n2, min n1' n2')
        else 
            assert ((n1 < n2 && n1' < n2') || (n1 > n2 && n1' > n2'))
            None
    let overlaps = offBounds |> Array.choose (fun off -> 
        match intersection onBounds.Rx off.Rx with
        | Some(x, x') -> 
            match intersection onBounds.Ry off.Ry with
            | Some(y, y') -> 
                match intersection onBounds.Rz off.Rz with
                | Some(z, z') -> Some({ Rx = (x, x'); Ry = (y, y'); Rz = (z, z'); })
                | None -> None
            | None -> None
        | None -> None)

    let (x, x') = onBounds.Rx
    let (y, y') = onBounds.Ry
    let (z, z') = onBounds.Rz
    overlaps |> Array.map (fun overlap ->  
        let (ox, ox') = overlap.Rx
        let (oy, oy') = overlap.Ry
        let (oz, oz') = overlap.Rz
        [|
            // top

            { Rx = (x, ox); Ry = (oy', y'); Rz = (oz', z'); };
            { Rx = (ox, ox'); Ry = (oy', y'); Rz = (oz', z'); };
            { Rx = (ox', x'); Ry = (oy', y'); Rz = (oz', z'); };

            { Rx = (x, ox); Ry = (oy', y'); Rz = (oz, oz'); };            
            { Rx = (ox', x'); Ry = (oy', y'); Rz = (oz, oz'); };

            { Rx = (x, ox); Ry = (oy', y'); Rz = (z, oz); };
            { Rx = (ox, ox'); Ry = (oy', y'); Rz = (z, oz); };
            { Rx = (ox', x'); Ry = (oy', y'); Rz = (z, oz); };

            // middle 

            { Rx = (x, ox); Ry = (oy, oy'); Rz = (oz', z'); };
            { Rx = (ox, ox'); Ry = (oy, oy'); Rz = (oz', z'); };
            { Rx = (ox', x'); Ry = (oy, oy'); Rz = (oz', z'); };

            { Rx = (x, ox); Ry = (oy, oy'); Rz = (oz, oz'); };            
            { Rx = (ox', x'); Ry = (oy, oy'); Rz = (oz, oz'); };

            { Rx = (x, ox); Ry = (oy, oy'); Rz = (z, oz); };
            { Rx = (ox, ox'); Ry = (oy, oy'); Rz = (z, oz); };
            { Rx = (ox', x'); Ry = (oy, oy'); Rz = (z, oz); };

            // bottom

            { Rx = (x, ox); Ry = (y, oy); Rz = (oz', z'); };
            { Rx = (ox, ox'); Ry = (y, oy); Rz = (oz', z'); };
            { Rx = (ox', x'); Ry = (y, oy); Rz = (oz', z'); };

            { Rx = (x, ox); Ry = (y, oy); Rz = (oz, oz'); };            
            { Rx = (ox', x'); Ry = (y, oy); Rz = (oz, oz'); };

            { Rx = (x, ox); Ry = (y, oy); Rz = (z, oz); };
            { Rx = (ox, ox'); Ry = (y, oy); Rz = (z, oz); };
            { Rx = (ox', x'); Ry = (y, oy); Rz = (z, oz); };
        |]
        )
        |> Array.collect id
        |> Array.where (fun cuboid -> computeSize cuboid > 0UL)

// Computes the sequence of bounds of which the given step overlaps with the given sequence of steps
let rec computeOverlaps (onBounds: Cuboid) (offBounds: Cuboid[]) =
    let intersection (n1, n1') (n2, n2') =
        if (n1 <= n2 && n1' >= n2') ||
           (n2 <= n1 && n2' >= n1') ||
           (n1 <= n2 && n1' >= n2) ||
           (n1 <= n2' && n1' >= n2') then 
            Some(max n1 n2, min n1' n2')
        else 
            assert ((n1 < n2 && n1' < n2') || (n1 > n2 && n1' > n2'))
            None
    let overlaps = offBounds |> Array.choose (fun off -> 
        match intersection onBounds.Rx off.Rx with
        | Some(x, x') -> 
            match intersection onBounds.Ry off.Ry with
            | Some(y, y') -> 
                match intersection onBounds.Rz off.Rz with
                | Some(z, z') -> Some({ Rx = (x, x'); Ry = (y, y'); Rz = (z, z'); })
                | None -> None
            | None -> None
        | None -> None)
    // if overlaps.Length > 0 then
    //     printfn "%A has %i overlaps. They are:" onBounds overlaps.Length
    //     overlaps |> Array.iter (fun t -> printfn "\t%A" t)

    if overlaps.Length = 0 then computeSize onBounds
    //elif overlaps.Length = 1 then (computeSize onBounds) - (computeSize overlaps[0])
    else 
        let mutable overlappedSum = 0UL
        for i in 0 .. overlaps.Length - 1 do
            let temp = overlaps[i]
            let remaining = overlaps |> Array.removeAt i
            overlappedSum <- overlappedSum + (computeOverlaps temp remaining) 
        computeSize onBounds - overlappedSum

        // (computeSize onBounds) -
        // (overlaps
        //     |> Array.mapi (fun i overlap ->
        //         let overlaps' = overlaps |> Array.removeAt i
        //         computeOverlaps overlap overlaps')
        //     |> Array.sum)
    //(computeSize bounds) - (overlaps |> Array.sumBy computeSize)
    // let a = (computeSize onBounds) 
    // let b = (overlaps |> Array.sumBy computeSize)
    // printfn "a=%i b=%i" a b
    // a - b

let reboot2 steps = 
    
    let steps' = 
        steps |> Array.mapi (fun i step ->
            let temp = steps[i].Bounds
            let remaining = steps |> Array.removeAt i |> Array.map (fun step -> step.Bounds)
            split temp remaining)
              |> Array.mapi (fun i arr -> 
                Array.create arr.Length (fun j ->
                    { State = steps[i].State; Bounds = arr[j]; } ))
              |> Array.collect id

    // partition into on and off cuboid arrays
    let onBounds, offBounds = partition steps'

    // let onBounds = 
    //     onBounds |> Array.mapi (fun i _ ->
    //         let temp = onBounds[i]
    //         let remaining = onBounds |> Array.removeAt i
    //         split temp remaining)
    //             |> Array.collect id

    // let offBounds =
    //     offBounds |> Array.mapi (fun i _ ->
    //         let temp = offBounds[i]
    //         let remaining = offBounds |> Array.removeAt i
    //         split temp remaining)
    //     |> Array.collect id

    let sums = onBounds |> Array.Parallel.map (fun bounds -> computeSize bounds - (computeOverlaps bounds offBounds))
    sums |> Array.sum
    // let tempSum = onBounds |> Array.sumBy computeSize
    // printfn "%i" (tempSum - sum)
    
    //printfn "there are %i cubes; %i overlaps" onBounds.Length overlaps.Length
    //let sums = onBounds |> Array.Parallel.map (fun bounds -> computeSize2 bounds overlaps)
    //sums |> Array.sum
    //42

    // compute sum of all "on" cuboids
    // let (onCount, _) =
    //     onSteps |> Array.fold (fun (onCount, onSteps) onStep -> 
    //         (onCount + (computeSize onStep.Bounds) - (overlap onStep onSteps), (onStep :: onSteps))) (0UL, List.empty)

    // //let initialOnCount = onSteps |> Array.sumBy (fun step -> computeSize step.Bounds)

    // let (offCount, _) =
    //     offSteps |> Array.fold (fun (offCount, offSteps) offStep -> 
    //         (offCount + (overlap offStep onSteps) - (overlap offStep offSteps), (offStep :: offSteps))) (0UL, List.empty)

    // printfn "%i" onCount
    // printfn "%i" offCount
    // onCount - offCount

let sampleOn = 
    System.IO.File.ReadAllLines "sample2.txt"
    |> Array.map parseRebootStep
    |> reboot2

printfn "%i" sampleOn
//t: 2758514936282235
//1: 2441245014278670
//2: 2276695655522924
