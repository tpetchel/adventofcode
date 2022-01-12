// https://adventofcode.com/2021/day/19
open System

// --- Day 19: Beacon Scanner ---

// This one really kicked my ass. Some parts aren't pretty but I'm learning.
// This is the first one I had to look for hints on Reddit. Primarily: 
// 1) How to compute the 24 rotations.
// 2) The insight to use distance squared when tracking beacon distances between scanners.
//    I'm not sure why but it works. ¯\_(ツ)_/¯

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
        { Scanner.Beacons = beacons; Index = scannerIndex - 1; }
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
        | Some(_) -> Some(scanners[index])
        | None -> None)

let tryAlignScanners (locatedScanner: Scanner) (scannerRotated: Scanner) =
    let distances =
        Array.allPairs locatedScanner.Beacons scannerRotated.Beacons
        |> Array.fold (fun distances (v1, v2) ->
            let distanceSquared = euclidianDistanceSquared v1 v2
            distances |> Map.change distanceSquared (fun v -> 
                match v with 
                | Some(lst) -> Some((v1,v2) :: lst)
                | None -> Some([(v1,v2)]))) Map.empty
    let pairs = distances |> Map.values |> Seq.maxBy (fun pairs -> pairs.Length)
    if pairs.Length >= 12 then
        let v0 = fst pairs[0]
        let v1 = snd pairs[0]
        Some(scannerRotated, new Vector3(v0.X - v1.X, v0.Y - v1.Y, v0.Z - v1.Z))
    else None

let printScanners scanners = 
    scanners |> Array.iter (fun (scanner: Scanner) ->
        printfn $"--- Scanner {scanner.Index} ---"
        scanner.Beacons |> Array.iter (fun beacon -> printfn $"{beacon}"))
    printfn "---"

let locateScanners (scanners: Scanner[]) =
    // TODO:This function is long, sloppy, and not very functional.
    // But it works and I'm tired.
    let mutable locations =
        { 0 .. (scanners.Length - 1) } |> Seq.map (fun index ->
            if index = 0 then (index, Some(Vector3(0,0,0)))
            else (index, None))
        |> Map.ofSeq

    let updateLocations locations index (location: Vector3) =
        locations |> Map.change index (fun _ -> Some(Some(location)))

    let mutable queue = scanners |> Array.skip 1
    while not (queue |> Array.isEmpty) do
        printfn $"Q has {queue |> Array.length} items"
        queue <- queue |> Array.choose (fun (scanner: Scanner) ->
            let rotations = scannerRotations scanner 
            let locatedScanners = (locatedScanners scanners locations)
            let alignedScanner = locatedScanners |> Array.tryPick (fun locatedScanner -> 
                match rotations |> Array.tryPick (fun rotation -> tryAlignScanners locatedScanner rotation) with
                | Some(scannerRotated,v) -> Some(locatedScanner,scannerRotated,v)
                | None -> None)
            match alignedScanner with
            | Some(locatedScanner, scannerRotated, v) -> 
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
    (locations |> Map.values |> Seq.choose id |> Seq.toArray, allPoints |> Array.distinct)

// --- Part One ---

let (sampleScanners, sampleBeacons) = loadReport "sample.txt" |> locateScanners
let (puzzleScanners, puzzleBeacons) = loadReport "input.txt" |> locateScanners

printfn $"{sampleBeacons.Length}" // 79
printfn $"{puzzleBeacons.Length}" // 442

// --- Part Two ---

// Easy!

let manhattanDistance ((v0, v1) : Vector3 * Vector3) =
    int (abs (v0.X - v1.X)) + (abs (v0.Y - v1.Y)) + (abs (v0.Z - v1.Z))

let greatestDistance locations = 
    Array.allPairs locations locations |> Array.map manhattanDistance |> Array.max

printfn $"{greatestDistance sampleScanners}" // 3621
printfn $"{greatestDistance puzzleScanners}" // 11079
