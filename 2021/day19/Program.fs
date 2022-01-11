// https://adventofcode.com/2021/day/19
open System
//open System.Numerics

// --- Day 19: Beacon Scanner ---

(*
for each scanner
    find the scanner this one overlaps with
    remember overlapping points

for each scanner pair, (s1,s2)
    for each orientation, O
        transform each beacon according to O
        if the distance, D, between each overlapping distance is the same,
            then s2 is at -D, relative to s1

for each scanner
    normalize points relative to scanner[0]

Answer <- count (distinct points)
*)

type Beacon = 
    struct
       val X : int
       val Y : int
       val Z : int
       new(x, y, z) = { X = x; Y = y; Z = z }
       override this.ToString() = $"<{this.X},{this.Y},{this.Z}>"
    end

// type Node = {
//     Value: Beacon; Edges: List<Beacon * int> }

// TODO: Convert Map below to 2D array
// Beacon | 0 1 2 3
//        +-------
//        0 0 n m ...
//        1 n
// ... and then return (i,j) pairs to indicate matches ...
// OR - graph DS with index[(v1,v2)] operator

// type Scanner = {
//     Beacons: Beacon[]; Distances: Map<int, (Beacon * Beacon)> }

type Scanner = { Beacons: Beacon[]; Distances: int[,]; }

let (|EmptyString|_|) s = if String.IsNullOrEmpty(s) then Some () else None

let permuteVectorRotation (v: Beacon) =
    (*
    // rotate around z
    ( x, y, z) -> ( 1, 2, 3) -> ( 1, 2, 3) -> ( x, y, z) (rotation 0)
    ( x, y, z) -> ( 1, 2, 3) -> (-2, 1, 3) -> (-y, x, z) (rotation 1)
    ( x, y, z) -> ( 1, 2, 3) -> (-1,-2, 3) -> (-x,-y, z) (rotation 2)
    ( x, y, z) -> ( 1, 2, 3) -> ( 2,-1, 3) -> ( y,-x, z) (rotation 3)
    // rotate around y
    ( x, y, z) -> ( 1, 2, 3) -> ( 1, 2, 3) -> ( x, y, z) (rotation 0)
    ( x, y, z) -> ( 1, 2, 3) -> (-3, 2, 1) -> (-z, y, x) (rotation 1)
    ( x, y, z) -> ( 1, 2, 3) -> (-1, 2,-3) -> (-x, y,-z) (rotation 2)
    ( x, y, z) -> ( 1, 2, 3) -> ( 3, 2,-1) -> ( z, y,-x) (rotation 3)
    // rotate around x
    ( x, y, z) -> ( 1, 2, 3) -> ( 1, 2, 3) -> ( x, y, z) (rotation 0)
    ( x, y, z) -> ( 1, 2, 3) -> ( 1,-3, 2) -> ( x,-z, y) (rotation 1)
    ( x, y, z) -> ( 1, 2, 3) -> ( 1,-2,-3) -> ( x,-y,-z) (rotation 2)
    ( x, y, z) -> ( 1, 2, 3) -> ( 1, 3,-2) -> ( x, z,-y) (rotation 3)
    *)
    let x = v.X
    let y = v.Y
    let z = v.Z
    [|
        new Beacon( x, y, z); // no rotation
        new Beacon(-y, x, z); new Beacon(-x,-y, z); new Beacon( y,-x, z); // z
        new Beacon(-z, y, x); new Beacon(-x, y,-z); new Beacon( z, y,-x); // y
        new Beacon( x,-z, y); new Beacon( x,-y,-z); new Beacon( x, z,-y); // x
    |]

let manhattan (v0: Beacon) (v1: Beacon) =
    (abs (v0.X - v1.X)) + (abs (v0.Y - v1.Y)) + (abs (v0.Z - v1.Z)) |> int

// let permuteOrientations =
//     let standardBasis = [|
//         new Vector3( 1f, 0f, 0f);
//         new Vector3( 0f, 1f, 0f);
//         new Vector3( 0f, 0f, 1f);
//         new Vector3(-1f, 0f, 0f);
//         new Vector3( 0f,-1f, 0f);
//         new Vector3( 0f, 0f,-1f);
//     |]
//     let axis = new Vector3(1f, 1f, 1f)
//     let rotations = { 0 .. 3 } |> Seq.map (fun i ->
//         let radians = ((Math.PI / 180.0) * (i |> float) * 90.0) |> float32
//         Quaternion.CreateFromAxisAngle (axis, radians)) |> Seq.toArray
//     let count1 = standardBasis |> Array.length
//     let count2 = rotations |> Array.length
//     seq {
//         for i = 0 to (count1 - 1) do
//             for j = 0 to (count2 - 1) do
//                 yield (standardBasis[i], rotations[j])
//     }

// let buildDistanceMap (points: Beacon[]) =
//     let permutePoints points =
//         let count = points |> Array.length
//         seq {
//             for i = 0 to (count - 1) do
//                 for j = (i + 1) to (count - 1) do
//                     yield (points[i], points[j])
//         }
//     let vals = permutePoints points
//     let keys = vals |> Seq.map (fun (v0, v1) -> manhattan v0 v1)
//     Seq.zip keys vals |> Map

let buildDistanceMap (points: Beacon[]) =
    let count = points |> Array.length
    let arr = Array2D.zeroCreate count count
    for i = 0 to (count - 1) do
        for j = i + 1 to (count - 1) do
            arr[i,j] <- manhattan points[i] points[j]
    arr

let loadReport filename =
    let parseScanner currScanner =
        let beacons =
            currScanner
            |> List.rev
            |> List.skip 1
            |> List.map (fun (line: string) -> 
                match line.Split(',') with
                | [|x;y;z|] -> new Beacon(x |> int, y |> int, z|> int)
                | _ -> failwith "Expected [x,y,z] array")
            |> List.toArray
        { Scanner.Beacons = beacons; Distances = buildDistanceMap beacons }
    let rec readScanners (currScanner: string list) lines =
        seq {
            match lines with
            | head :: tail -> 
                match head with
                | EmptyString ->
                    yield parseScanner currScanner
                    yield! readScanners [] tail
                | _ -> yield! readScanners (head :: currScanner) tail
            | [] -> yield parseScanner currScanner
        }
    System.IO.File.ReadAllLines filename |> Array.toList |> readScanners [] |> Seq.toArray

let findOverlappingPoints (scanner1: Scanner) (scanner2: Scanner) =
    let rows = scanner1.Distances |> Array2D.length1
    let cols = scanner1.Distances |> Array2D.length2

    let rec nextPoint (i1,j1) (i2,j2) pairs =
        if i2 = rows then nextPoint (i1,j1) (0,j2+1) pairs
        elif j2 = cols then pairs
        elif scanner1.Distances[i1,j1] = scanner2.Distances[i2,j2] && scanner1.Distances[i1,j1] <> 0 then
            let pairs' = (scanner1.Beacons[i1], scanner2.Beacons[i2]) :: (scanner1.Beacons[j1], scanner2.Beacons[j2]) :: pairs
            nextPoint (i1,j1) (i2+1,j2) (pairs')
        else
            nextPoint (i1,j1) (i2+1,j2) pairs
    { 0 .. rows - 1 } |> Seq.fold (fun pairs i ->
        { 0 .. cols - 1 } |> Seq.fold (fun pairs j -> nextPoint (i,j) (0,0) pairs) pairs) []
    |> List.toArray

// --- Part One ---

let scanners = loadReport "sample.txt"

scanners |> Array.iteri (fun i (scanner: Scanner) ->
    printfn $"--- Scanner {i} ---"
    scanner.Beacons
    |> Array.iter (fun beacon -> printfn $"{beacon}"))

// let orientations = permuteOrientations |> Seq.toArray
// orientations |> Array.iter (fun (forward, up) ->
//     printfn $"forward -> {forward} up -> {up}")


let scanner0 = scanners[0]
let scanner1 = scanners[1]

let overlappingPairs = findOverlappingPoints scanner0 scanner1

overlappingPairs |> Array.iter (fun (b1, b2) ->
    printfn $"{b1} -> {b2}")
printfn $"{overlappingPairs.Length}"

let set0 = overlappingPairs |> Array.map (fun (a,_) -> a)
let set1 = overlappingPairs |> Array.map (fun (_,b) -> b)
let allBeaconRotations = set1 |> Array.map permuteVectorRotation
for i in 0 .. 9 do
    let distances = 
        (set0, allBeaconRotations |> Array.map (fun a -> a[i]))
        ||> Array.zip
        |> Array.map (fun (b1,b2) -> manhattan b1 b2)
        |> Array.groupBy (fun d -> d)
    distances |> Array.iter (fun (d, a) ->
        if a |> Array.length > 12 then
            printfn $"GGG! {d} {a |> Array.length}" )
    //if distances |> Array.forall (fun distance -> distance = distances[0]) then
    //    printfn $"iter {i}: All distances equal {distances[0]}!"

// allBeaconRotations |> Array.iteri (fun i beaconRotatons ->
//     let distances = 
//         (set0, beaconRotatons)
//         ||> Array.zip
//         |> Array.map (fun (b1,b2) -> manhattan b1 b2)
//     if distances |> Array.forall (fun distance -> distance = distances[0]) then
//         printfn $"iter {i}: All distances equal {distances[0]}!"
//     )

// for each orientation, O
//     transform each beacon according to O
//     if the distance, D, between each overlapping distance is the same,
//         then s2 is at -D, relative to s1

// let matches = scanner0.Distances.Values |> Seq.fold (fun matches distance ->
//     if scanner1.Distances.Values |> Seq.contains distance then
//         let pair = (scanner0.Distances |> Map.findKey (fun _ d -> d = distance),
//                     scanner1.Distances |> Map.findKey (fun _ d -> d = distance),
//                     distance)
//         pair :: matches
//     else matches) []

// matches |> Seq.iteri (fun i (p1, p2, d) ->
//     printfn $"{d} {p1} {p2}")

// let matchesInScanner0 =
//     matches
//     |> Seq.map (fun ((v1, v2), _, _) -> (v1, v2))
//     |> Seq.fold (fun lst (v1, v2) -> v1 :: v2 :: lst) []
//     |> Seq.distinct
// printfn "==="
// matchesInScanner0 |> Seq.iteri (fun i v ->
//     printfn $"{i}: {v}")

// Triangulate
// printfn "==="
// matches |> List.iter (fun ((v1, v2),(v3, v4),d) ->
//     match matches |> List.tryFind (fun ((v1', v2'),(v3', v4'),d') -> 
//         if (v1, v2) = (v1', v2') || (v3, v4) = (v3', v4') then false
//         elif (v1 = v1' || v2 = v2') then true
//         elif (v3 = v3' || v4 = v4') then true
//         else false) with
//     | Some(n) -> printfn "==="
//     | None -> ()
//     )

// let countMatches v matches =
//     matches |> List.sumBy (fun ((v1, v2), (v3, v4), d) -> 
//         if v = v1 || v = v2 || v = v3 || v = v4 then 1
//         else 0)

// matches |> List.iter (fun ((v1, v2), (v3, v4), d) ->
//     let c1 = countMatches v1 matches
//     let c2 = countMatches v2 matches
//     let c3 = countMatches v3 matches
//     let c4 = countMatches v4 matches
//     printfn $"{c1}"
//     printfn $"{c2}"
//     printfn $"{c3}"
//     printfn $"{c4}")


    // match scanner1.Distances |> Map.tryFindKey (fun _ v' -> v = v') with
    // | Some((v1', v2')) -> 
    //     let t1 = scanner0.Distances |> Map.tryFindKey (fun (a,b) v' ->
    //         = v')
    // | None -> ()
    // )

//scanner0.Distances |> Map.pick (fun _ v -> scanner1.Distances.Values |> Seq.contains v)
//printfn "%A %i" matches (matches |> Seq.length)

// let count0 = scanner0.Distances.Keys.Count
// let count1 = scanner1.Distances.Keys.Count
// for i = 0 to (count0 - 1) do
//     for j = 0 to (count1 - 1) do
//         if 
//             yield (points[i], points[j])

(*
let radians i = ((Math.PI / 180.0) * (i |> float) * 90.0) |> float32

let s0 = scanner[0]
s0.Position <- (0,0,0)
Add scanner points to knowledge map

scanners' = scanners.skip 1
for each scanner s in scanners':
    for each scanner s' in scanners':
        if s <> s' then
            foreach beacon b in s
                apply each of the 24 transformations
            foreach transformed position, compute distance to ith beacon in s'
            if > 12 distances match, you found an overlap!
                Add normlized points to knowledge map
                s'.Position <- common distance
                Add scanner points to knowledge map
return unique points in knowledge map

//let dist (v0: Vector3) (v1: Vector3) =
//    sqrt (((v0.X - v1.X)**2f) + ((v0.Y - v1.Y)**2f) + ((v0.Z - v1.Z)**2f))


let doSomething arr =
    let distances = arr |> Array.map (fun (v0, v1) -> 

        let rotations = permuteVectorRotation v1
        
        // How many rotations are in the reference cube?
        // let center = Vector3.Zero
        // let t = rotations |> Array.mapi (fun i rotation ->
        //     let d = floor (dist center rotation) |> int
        //     //printfn $"{v0} -> {v1}: {rotation} ({d})"
        //     (i, d))
        // //t |> Array.iter (fun (i,d) -> printfn $"{i} -> {d}")
        // let overlapping = t |> Array.choose (fun (i, d) -> 
        //     if d < 1000 then Some(i,d); else None)
        // printfn "%i" (Array.length overlapping)

        // let distances = rotations |> Array.mapi (fun i rotation ->
        //     let d = floor (dist v0 rotation) |> int
        //     printfn $"{v0} -> {v1}: {rotation} ({d})"
        //     (i, d))
        // let groups = distances |> Array.groupBy (fun (_, d) -> d)
        // groups
        let distances = rotations |> Array.mapi (fun i rotation -> manhattan v0 rotation)
        distances)
    distances

let arr1 = Array.zip scanner0.Beacons scanner1.Beacons
let arr2 = Array.zip scanner0.Beacons (scanner1.Beacons |> Array.rev)

let distances = Array.append (doSomething arr1) (doSomething arr2)

distances |> Array.iteri (fun i distance -> 
    printfn "pair %i:" i
    distance |> Array.iteri (fun i d -> 
        printfn "%i %i" i d)
    )

// let t = { 0 .. 9 } |> Seq.map (fun i ->
//     distances |> Array.map (fun distance -> distance[i]))

// t |> Seq.iteri (fun i arr ->
//     printfn "%i: %A" i (arr |> Array.sort)
//     )

let m = Array2D.zeroCreate distances.Length 10
for i in 0 .. (distances.Length - 1) do
  for j in 0 .. 9 do
    m[i,j] <- distances[i][j]
    printfn "%i" (distances[i][j])
//printfn "%A" m

// for j in 0 .. 9 do
//     let groups = m[*,j] |> Array.groupBy (fun x -> x)
//     groups |> Array.iteri (fun i (n, arr) ->
//         printfn "group %i: %i %i" i n arr.Length)

// for j in 0 .. (distances.Length - 1) do
//     let groups = m[j,*] |> Array.groupBy (fun x -> x)
//     groups |> Array.iteri (fun i (n, arr) ->
//         printfn "group %i: %i %i" i n arr.Length)
*)

// --- Part Two ---
