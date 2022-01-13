// https://adventofcode.com/2021/day/20

// --- Day 20: Trench Map ---

let (|One|Zero|) input = if input = '#' then One else Zero

let lookupBit (bitTable: bool[]) index = bitTable[index]

let readInput filename = 
    let mapRow (row: string) =
        row.ToCharArray()
        |> Array.map(fun ch -> 
            match ch with
            | One -> true
            | Zero -> false)
    let lines = System.IO.File.ReadAllLines(filename)
    let bitTable = mapRow lines[0]
    let inputImage = lines |> Array.skip 2 |> Array.map mapRow |> array2D
    (lookupBit bitTable, inputImage)

let expandImage count (image: bool[,]) =
    let len1 = image |> Array2D.length1
    let len2 = image |> Array2D.length2
    let arr = Array2D.zeroCreate (len1 + count) (len2 + count)
    Array2D.blit image 0 0 arr (count/2) (count/2) len1 len2
    arr

let printImage (image: bool[,]) =
    for i = 0 to (image |> Array2D.length1) - 1 do
        for j = 0 to (image |> Array2D.length2) - 1 do
            if image[i,j] then printf "#" else printf "."
        printfn ""

let bitsToDecimal (bits: bool[]) =
    bits |> Array.rev |> Array.mapi (fun k n -> if n then 1 <<< k else 0) |> Array.sum

let kernel i j (image: bool[,]) = 
    let maxI = (image |> Array2D.length1) - 1
    let maxJ = (image |> Array2D.length2) - 1
    let rec fetch i j = 
        if i < 0 then fetch 0 j
        elif i > maxI then fetch maxI j
        elif j < 0 then fetch i 0
        elif j > maxJ then fetch i maxJ
        else image[i,j]
    [| 
        fetch (i-1) (j-1); fetch (i-1) j; fetch (i-1) (j+1);
        fetch     i (j-1); fetch     i j; fetch     i (j+1);
        fetch (i+1) (j-1); fetch (i+1) j; fetch (i+1) (j+1);
    |]

let countLitPixels (image: bool[,]) =
    let len1 = image |> Array2D.length1
    let len2 = image |> Array2D.length2
    let mutable sum = 0
    for i = 0 to len1 - 1 do
        for j = 0 to len2 - 1 do
            if image[i,j] then sum <- sum + 1
    sum

let enhanceImage lookup inputImage =
    //let inputImage = expandImage inputImage
    let len1 = inputImage |> Array2D.length1
    let len2 = inputImage |> Array2D.length2
    let outputImage = Array2D.zeroCreate len1 len2
    for i = 0 to len1 - 1 do
        for j = 0 to len2 - 1 do
            let index = inputImage |> kernel i j |> bitsToDecimal
            outputImage[i,j] <- lookup index
    outputImage

let enhanceImageBy lookup inputImage times =
    { 1 .. times } |> Seq.fold (fun image _ -> enhanceImage lookup image) (expandImage (2*times) inputImage)

// --- Part One ---

let sampleLookup, sampleInputImage = readInput "sample.txt"
let sampleResult = enhanceImageBy sampleLookup sampleInputImage 2
printImage sampleResult
printfn $"{countLitPixels sampleResult}" // 35

printfn "==="

let lookup, inputImage = readInput "input.txt"
let result = enhanceImageBy lookup inputImage 2
printImage result
printfn $"{countLitPixels result}" // 5682

// --- Part Two ---

let sampleResult' = enhanceImageBy sampleLookup sampleInputImage 50
printImage sampleResult'
printfn $"{countLitPixels sampleResult'}" // 3351

printfn "==="

let result' = enhanceImageBy lookup inputImage 50
printImage result'
printfn $"{countLitPixels result'}" // 17628