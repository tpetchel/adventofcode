// https://adventofcode.com/2021/day/20
open System

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

let expandImage (image: bool[,]) =
    let len1 = image |> Array2D.length1
    let len2 = image |> Array2D.length2
    let arr = Array2D.zeroCreate (len1 + 2) (len2 + 2)
    Array2D.blit image 0 0 arr 1 1 len1 len2
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
    let inline fetch i j = 
        if i < 0 || j < 0 || i > maxI || j > maxJ then false
        else image[i,j]
    [| 
        fetch (i-1) (j-1); fetch (i-1) j; fetch (i-1) (j+1);
        fetch     i (j-1); fetch     i j; fetch     i (j+1);
        fetch (i+1) (j-1); fetch (i+1) j; fetch (i+1) (j+1);
    |]
    // [| 
    //     image[i-1, j-1]; image[i-1, j]; image[i-1, j+1];
    //     image[i,   j-1]; image[i,   j]; image[i,   j+1];
    //     image[i+1, j-1]; image[i+1, j]; image[i+1, j+1];
    // |]

let countLitPixels (image: bool[,]) =
    let len1 = image |> Array2D.length1
    let len2 = image |> Array2D.length2
    let mutable sum = 0
    for i = 0 to len1 - 1 do
        for j = 0 to len2 - 1 do
            if image[i,j] then sum <- sum + 1
    sum

let enhanceImage lookup inputImage =
    let inputImage = expandImage inputImage
    let len1 = inputImage |> Array2D.length1
    let len2 = inputImage |> Array2D.length2
    let outputImage = Array2D.zeroCreate len1 len2
    for i = 0 to len1 - 1 do
        for j = 0 to len2 - 1 do
            let index = inputImage |> kernel i j |> bitsToDecimal
            outputImage[i,j] <- lookup index
    outputImage

let enhanceImageBy lookup inputImage times =
    { 1 .. times } |> Seq.fold (fun image _ -> enhanceImage lookup image ) inputImage
    // let rec loop image times =
    //     if times = 0 then image
    //     else loop (enhanceImage lookup image) (times - 1)
    // loop inputImage times

// --- Part One ---

let sampleLookup, sampleInputImage = readInput "sample.txt"
let sampleResult = enhanceImageBy sampleLookup sampleInputImage 2
printImage sampleResult
printfn $"{countLitPixels sampleResult}" // 35

printfn "==="

let lookup, inputImage = readInput "input.txt"
let result = enhanceImageBy lookup inputImage 2
printImage result
printfn $"{countLitPixels result}" // 6196, 5776, 5552, 6532

// printImage sampleInputImage
// printfn "---"
// let sampleInputImage' = enhanceImage sampleLookup sampleInputImage
// printImage sampleInputImage'
// printfn $"{countLitPixels sampleInputImage'}"
// printfn "---"
// let sampleInputImage'' = enhanceImage sampleLookup sampleInputImage'
// printImage sampleInputImage''
// printfn $"{countLitPixels sampleInputImage''}"

// printfn $"{bitsToDecimal [|false|] }"
// printfn $"{bitsToDecimal [|true|] }"
// printfn $"{bitsToDecimal [|true;false;|] }"
// printfn $"{bitsToDecimal [|true;true;|] }"
// printfn $"{bitsToDecimal [|true;true;true;true|] }"
// //printfn $"{bitsToDecimal [|1;1;1;1;1;1;1;1;1;|] }"

// --- Part Two ---