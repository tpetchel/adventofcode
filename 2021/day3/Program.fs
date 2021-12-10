// https://adventofcode.com/2021/day/3

// --- Day 3: Binary Diagnostic ---
let readInput file = System.IO.File.ReadAllLines(file) |> array2D

// Returns the element that appears most often
let occursMost arr =
    let counts = arr |> Array.countBy (fun e -> e)
    counts |> Array.maxBy (fun (_, count) -> count) |> (fun (e, _) -> e)

// Flips 0's to 1 and 1's to 0.
let flipBits bits = bits |> Seq.map (fun bit ->
    match bit with
    | '0' -> '1'
    | _ -> '0')

// Prints the given sequence
let printBits bits =
    bits |> Seq.iter (printf "%c")
    printfn ""

// Converts a sequence of character "bits" (0's and 1's)
// to an int.
let toNumber s numBits = 
    let toBit bit =
        match bit with
        | '0' -> 0
        | _ -> 1
    s |> Seq.mapi (fun n ch -> 
        let number = ch |> toBit |> int
        let shiftBy = numBits - n - 1
        number <<< shiftBy)
    |> Seq.sum

// Read input data as a 2D array.
let input = readInput "input.txt"

// Get the number of columns in the array.
let numColumns = input |> Array2D.length2

// Compute gamma by counting the "bit" that occurs most often in each column.
let gammaBits = seq { 0 .. numColumns-1 } |> Seq.map (fun i -> occursMost input[*,i])
// Flip each bit to produce epsilon.
let epsilonBits = flipBits gammaBits

// Print binary strings.
gammaBits |> printBits
epsilonBits |> printBits

// Convert binary strings to numbers and print their product.
let gamma = toNumber gammaBits numColumns
let epsilon = toNumber epsilonBits numColumns
printfn "%i" (gamma * epsilon)

// --- Part Two ---
let allSame arr = 
    match Array.length arr with
    | 0 | 1 -> true
    | _ -> arr |> Array.skip 1 |> Array.forall ((=) (Array.head arr))

let occursMost' arr extremum defaultIfTie =
    let counts = arr |> Array.countBy (fun e -> e)
    let countsOnly = counts |> Array.map (fun (_, count) -> count)
    if allSame countsOnly then defaultIfTie
    else counts |> extremum (fun (_, count) -> count) |> (fun (e, _) -> e)

let filter (arr2D:'a[,]) column extremum defaultIfTie = 
    let columnArray = arr2D[*,column]
    let filterChar = occursMost' columnArray extremum defaultIfTie
    // Convert 2D array to array of arrays so that we can 
    // more easily filter and reduce.
    let temp = Array.init (arr2D |> Array2D.length1) (fun i -> arr2D[i,*])
    temp |> Array.where (fun line -> line[column] = filterChar) |> array2D

let rating input extremum defaultIfTie =
    let rec reduce arr2D column = 
        match arr2D |> Array2D.length1 with
        | 0 -> failwith "empty"
        | 1 -> arr2D[0,*]
        | _ -> reduce (filter arr2D column extremum defaultIfTie) (column+1)
    reduce input 0

printfn "--- Part Two ---"

let oxygenRatingBits = rating input Array.maxBy '1'
let co2RatingBits = rating input Array.minBy '0'

// Print binary strings.
oxygenRatingBits |> printBits
co2RatingBits |> printBits

let o2Rating = toNumber oxygenRatingBits numColumns
let co2Rating = toNumber co2RatingBits numColumns
printfn $"{o2Rating * co2Rating}"