// https://adventofcode.com/2021/day/19
open System

// --- Day 19: Beacon Scanner ---

type Vector3 = 
    struct
       val X : int
       val Y : int
       val Z : int
       new(x, y, z) = { X = x; Y = y; Z = z }
       override this.ToString() = $"<{this.X},{this.Y},{this.Z}>"
    end

type Scanner = { Beacons: Vector3[]; Index: int; }

let rotateAxisAligned (v: Vector3) rotationIndex =
    let rotateCoords (v: Vector3) =
        let (x, y, z) = (v.X, v.Y, v.Z)
        match (rotationIndex % 6) with
        | 0 -> new Vector3(x, y, z)
        | 1 -> new Vector3(-x, y, -z)
        | 2 -> new Vector3(y, -x, z)
        | 3 -> new Vector3(-y, x, z)
        | 4 -> new Vector3(z, y, -x)
        | 5 -> new Vector3(-z, y, x)
        | _ -> failwith "?"
    let rotateXAxis (v: Vector3) =
        let (x, y, z) = (v.X, v.Y, v.Z)
        match ((rotationIndex / 6) % 4) with
        | 0 -> new Vector3(x, y, z)
        | 1 -> new Vector3(x, -z, y)
        | 2 -> new Vector3(x, -y, -z)
        | 3 -> new Vector3(x, z, -y)
        | _ -> failwith "?"
    rotateXAxis (rotateCoords v)

let scannerRotations (s: Scanner) =
    Array.init 24 (fun rotationIndex ->
        let rotated = s.Beacons |> Array.map (fun beacon -> rotateAxisAligned beacon rotationIndex)
        { Scanner.Beacons = rotated; Index = s.Index; })

let euclidianDistanceSquared (v0: Vector3) (v1: Vector3) =
    let x = (v1.X - v0.X) * (v1.X - v0.X)
    let y = (v1.Y - v0.Y) * (v1.Y - v0.Y)
    let z = (v1.Z - v0.Z) * (v1.Z - v0.Z)
    x + y + z

let manhattanDistance (v0: Vector3) (v1: Vector3) =
    (abs (v0.X - v1.X)) + (abs (v0.Y - v1.Y)) + (abs (v0.Z - v1.Z)) |> int

let loadReport filename =
    let (|EmptyString|_|) s = if String.IsNullOrEmpty(s) then Some () else None
    let mutable scannerIndex = 0
    let parseScanner currScanner =
        let beacons =
            currScanner
            |> List.rev
            |> List.skip 1
            |> List.map (fun (line: string) -> 
                match line.Split(',') with
                | [|x;y;z|] -> new Vector3(x |> int, y |> int, z|> int)
                | _ -> failwith "Expected [x,y,z] array")
            |> List.toArray
        scannerIndex <- scannerIndex + 1
        { Scanner.Beacons = beacons; Index = scannerIndex - 1; }// RotationIndex = 0; }
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

let locatedScanners (scanners: Scanner[]) locations =
    locations |> Map.toArray |> Array.choose (fun (index, v) -> 
        match v with
        | Some(v) -> Some(scanners[index])
        | None -> None)

let matchPoints (locatedScanner: Scanner) (scannerRotated: Scanner) =
    let mutable distances = Map<int, List<Vector3 * Vector3>> []
    for v1 in locatedScanner.Beacons do
        for v2 in scannerRotated.Beacons do
            let dist = euclidianDistanceSquared v1 v2
            distances <- distances |> Map.change dist (fun v -> 
                match v with 
                | Some(lst) -> Some((v1,v2) :: lst)
                | None -> Some([(v1,v2)]))
    let (dist, pairs) =
        distances
        |> Map.toArray 
        |> Array.maxBy (fun (_, pairs) -> pairs |> List.length)
    if pairs.Length >= 12 then
        let v0 = fst pairs[0]
        let v1 = snd pairs[0]
        Some(scannerRotated, new Vector3(v0.X - v1.X, v0.Y - v1.Y, v0.Z - v1.Z))
    else None

let doIt scanners =
    scanners |> Array.iteri (fun i (scanner: Scanner) ->
        printfn $"--- Scanner {scanner.Index} ---"
        scanner.Beacons |> Array.iter (fun beacon -> printfn $"{beacon}"))

    let mutable locations =
        { 0 .. (scanners.Length - 1) } 
        |> Seq.map (fun index ->
            if index = 0 then (index, Some(Vector3(0,0,0)))
            else (index, None))
        |> Map.ofSeq

    let updateLocations locations index (location: Vector3) =
        locations |> Map.change index (fun _ -> Some(Some(location)))

    printfn "----"

    let mutable queue = scanners |> Array.skip 1
    while not (queue |> Array.isEmpty) do
        printfn $"Q has {queue |> Array.length} items"
        queue <- queue |> Array.choose (fun (scanner: Scanner) ->
            let rotations = scannerRotations scanner 
            let locatedScanners = (locatedScanners scanners locations)
            let xxx = locatedScanners |> Array.tryPick (fun locatedScanner -> 
                match rotations |> Array.tryPick (fun rotation -> matchPoints locatedScanner rotation) with
                | Some(scannerRotated,v) -> Some(locatedScanner,scannerRotated,v)
                | None -> None)
            match xxx with
            | Some(locatedScanner,scannerRotated,v) -> 
                printfn $"Scanner {scanner.Index} is at: {v} relative to {locatedScanner.Index}"
                let relPos = locations[locatedScanner.Index].Value
                let absPos = new Vector3(v.X + relPos.X, v.Y + relPos.Y, v.Z + relPos.Z)
                printfn $"\tAbsolute position is {absPos}"
                scanners[scanner.Index] <- scannerRotated//{ Scanner.Beacons = rotation; Index = scanner.Index; }
                locations <- updateLocations locations scanner.Index absPos
                None
            | None -> Some(scanner))

    let allPoints = 
        scanners
        |> Array.map (fun scanner -> 
            scanner.Beacons |> Array.map (fun b -> 
                let p = locations[scanner.Index].Value
                new Vector3(b.X + p.X, b.Y + p.Y, b.Z + p.Z)))
        |> Array.collect id
    (locations |> Map.values |> Seq.choose id, allPoints |> Array.distinct)

// --- Part One ---

let (scanners1, a) = loadReport "sample.txt" |> doIt
let (scanners2, b) = loadReport "input.txt" |> doIt

printfn $"{a.Length}"
printfn $"{b.Length}"

// --- Part Two ---

let greatestDistance locations = 
    let mutable greatest = -1
    for p1 in locations do
        for p2 in locations do
            let m = manhattanDistance p1 p2
            if m > greatest then
                greatest <- m
    greatest

printfn $"{greatestDistance scanners1}"
printfn $"{greatestDistance scanners2}"
