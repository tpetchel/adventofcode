// https://adventofcode.com/2021/day/18
open System

// --- Day 18: Snailfish ---

type Element =
    | Constant of int
    | Pair of Element * Element
    with override this.ToString() =
             match this with
             | Constant(n) -> n.ToString()
             | Pair(e1,e2) -> $"[{e1},{e2}]"

let constantValue (e: Element) = 
    match e with
    | Constant(c) -> c
    | _ -> failwith "Element is not a Constant"

// Inspiraton for recursive descent parser taken from http://www.fssnip.net/bM

let (|TL|_|) (prefix: string) (s: string) =
    let s = s.TrimStart()
    if s.StartsWith(prefix) then
        Some (s.Substring(prefix.Length))
    else
        None

let (|ConstExpr|_|) (str: string) =
   let mutable intvalue = 0
   let ch = str[0].ToString()
   if System.Int32.TryParse(ch, &intvalue) then Some(ch, str.Substring(1))
   else None

let rec (|PairExpr|_|) (s: string) =
    match s with
    // [const,const]
    | TL "[" (ConstExpr(n1, (TL "," (ConstExpr(n2, (TL "]" rest)))))) -> 
        Some ((Constant(Int32.Parse(n1)), Constant(Int32.Parse(n2))), rest)
    // [pair,pair]
    | TL "[" (PairExpr(n1, (TL "," (PairExpr(n2, (TL "]" rest)))))) -> 
        Some ((Pair(n1), Pair(n2)), rest)
    // [const,pair]
    | TL "[" (ConstExpr(n1, (TL "," (PairExpr(n2, (TL "]" rest)))))) -> 
        Some ((Constant(Int32.Parse(n1)), Pair(n2)), rest)
    // [pair,const]
    | TL "[" (PairExpr(n1, (TL "," (ConstExpr(n2, (TL "]" rest)))))) -> 
        Some ((Pair(n1), Constant(Int32.Parse(n2))), rest)
    | _ -> None

let parse s =
    match s with
    | PairExpr(p, _) -> Pair(p)
    | _ -> failwith "Syntax error"

let (|Explode|_|) (n: Element)  =
    let mutable foundMatch = false
    let rec explode n (leftSibling, rightSibling) nestLevel =
        //printfn $"{nestLevel} -> {n}"
        if foundMatch then (n, false)
        else
            match n with
            | Constant(_) -> (n, false)
            | Pair(e1, e2) -> 
                if nestLevel = 4 then
                    printfn $"Pair {n} explodes!"
                    foundMatch <- true

                    // The pair's left value is added to the first regular number to the left
                    // of the exploding pair (if any).
                    // The pair's right value is added to the first regular number to the right
                    // of the exploding pair (if any).
                    // Exploding pairs will always consist of two regular numbers.
                    // Then, the entire exploding pair is replaced with the regular number 0.
                    let c1 = constantValue e1
                    let c2 = constantValue e2

                    match leftSibling with
                    | Some(Constant c) -> (Pair(Constant(c + c1), Constant(0)), true)
                    | _ -> 
                        match rightSibling with
                        | Some(Constant c) -> (Pair(Constant(0), Constant(c + c2)), true)
                        | _ -> failwith "Expected left or right Constant sibling"
                else
                    let nestLevel = nestLevel + 1
                    match explode e1 (None, Some(e2)) nestLevel with
                    | (n, true) -> (n, false)
                    | (n, false) ->
                        match explode e2 (Some(e1), None) nestLevel with
                        | (m, true) -> (m, false)
                        | (m, false) -> (Pair(n, m), false)
    let (r, _) = explode n (None, None) 0
    if foundMatch then Some(r)
    else None

let (|Split|_|) (n: Element)  =
    // Even numbers are divisible by 2.
    let isEven x = (x % 2) = 0
    // Odd numbers are not even.
    let isOdd x = isEven x = false

    let mutable foundMatch = false
    let rec split n =
        if foundMatch then (n, false)
        else
            match n with
            | Constant(c) ->
                if c < 10 then
                    (n, false)
                else
                    printfn $"Constant {c} splits!"
                    foundMatch <- true
                    if isOdd c then
                        let t = (c + 1) / 2
                        (Pair(Constant(t - 1), Constant(t)), true)
                    else
                        let t = c / 2
                        (Pair(Constant(t), Constant(t)), true)
            | Pair(e1, e2) ->
                match split e1 with
                    | (n, true) -> (n, false)
                    | (n, false) ->
                        match split e2 with
                        | (m, true) -> (m, false)
                        | (m, false) -> (Pair(n, m), false)
    let (r, _) = split n
    if foundMatch then Some(r)
    else None

let total elements =
    let rec reduce (n: Element) =
        match n with
        | Explode(n') ->
            printfn $"After explode: {n'}"
            reduce(n')
        | Split(n') ->
            printfn $"After split: {n'}"
            reduce(n')
        | _ -> n
    let add (n1: Element) (n2: Element) =
        reduce (Pair (n1, n2))
    Array.reduce add elements

// --- Part One ---

// Sample data

let samples = [|
    "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]";
    "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]";
    "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]";
    "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]";
    "[7,[5,[[3,8],[1,4]]]]";
    "[[2,[2,2]],[8,[8,1]]]";
    "[2,9]";
    "[1,[[[9,3],9],[[9,0],[0,7]]]]";
    "[[[5,[7,4]],7],1]";
    "[[[[4,2],2],6],[8,7]]"; |] |> Array.map (fun s -> parse s)

//samples |> Array.iter (fun s -> printfn $"{s}")
//printfn "==="

// let sum = (add, samples) ||> Array.reduce
// printfn $"{sum}"

//let sum = total samples
//printfn $"{sum}"

printfn "==="
let t = parse "[[[[[9,8],1],2],3],4]"
let _ = total [| t |]
printfn $"{t}"

match t with
| Explode(t') | Split(t') -> printfn $"{t'}"
| _ -> printfn "None"


printfn "==="
let t' = parse "[7,[6,[5,[4,[3,2]]]]]"
let _ = total [| t' |]
printfn $"{t'}"

match t' with
| Explode(t'') | Split(t'') -> printfn $"{t''}"
| _ -> printfn "None"


printfn "==="
let t'' = parse "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
let _ = total [| t'' |]
printfn $"{t''}"

match t'' with
| Explode(t''') | Split(t''') -> printfn $"{t'''}"
| _ -> printfn "None"

printfn "==="

let x = parse "[[[[4,3],4],4],[7,[[8,4],9]]]"
let y = parse "[1,1]"
let z = total [| x; y |]
printfn $"{z}"

// [[[[0,7],4],[7,[6,7]]],[1,1]]
// [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
// Puzzle data

// --- Part Two ---

// Sample data

// Puzzle data
