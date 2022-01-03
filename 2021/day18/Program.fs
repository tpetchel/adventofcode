// https://adventofcode.com/2021/day/18
open System
open System.Text.RegularExpressions

// --- Day 18: Snailfish ---

type Element =
    | Constant of int
    | Pair of Element * Element
    with override this.ToString() =
             match this with
             | Constant(n) -> n.ToString()
             | Pair(e1,e2) -> $"[{e1},{e2}]"

// Some inspiraton for this recursive descent parser taken from
// http://www.fssnip.net/bM

let (|TL|_|) (prefix: string) (s: string) =
    let s = s.TrimStart()
    if s.StartsWith(prefix) then Some (s.Substring(prefix.Length))
    else None

let isDigit (ch: char) =
   let mutable intvalue = 0
   System.Int32.TryParse(ch.ToString(), &intvalue)

let (|ConstExpr|_|) (str: string) =
    let mutable intvalue = 0
    let digits = str |> Seq.takeWhile isDigit |> String.Concat
    if System.Int32.TryParse(digits, &intvalue) then Some(digits, str.Substring(digits.Length))
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

// I gave up trying to process this as a graph, as it's very difficult
// to access a node's lexical siblings, let alone update them cleanly.
// So here, we basically serialize the object to a string, update the string
// directly, and parse it back out.

let (|Explode|_|) (n: Element) =
    let extractPair (pair: string) i i' =
        let matched = Regex.Match(pair[i..i'], "\[(\d+)\,(\d+)\]")
        (Int32.Parse(matched.Groups.[1].Value), Int32.Parse(matched.Groups.[2].Value))

    let findNextNumber startIndex incrementBy (chars: char array) =
        let rec findStart currIndex incrementBy (chars: char array) =
            if currIndex < 0 || currIndex >= (chars |> Array.length) then None
            else
                if isDigit chars[currIndex] then Some(currIndex)
                else findStart (currIndex + incrementBy) incrementBy chars
        let rec findEnd currIndex incrementBy (chars: char array) =
            if currIndex < 0 || currIndex >= (chars |> Array.length) then (currIndex - incrementBy)
            else
                if isDigit chars[currIndex] then findEnd (currIndex + incrementBy) incrementBy chars
                else (currIndex - incrementBy)
        let start = findStart startIndex incrementBy chars
        match start with
        | Some(startIndex) ->
            let endIndex = findEnd (startIndex + incrementBy) incrementBy chars 
            Some(min startIndex endIndex, max startIndex endIndex)
        | None -> None
    let rec findPair currIndex lastStartIndex targetDepth leftCount (chars: char array) =
        if currIndex >= (chars |> Array.length) then None
        else
            match chars[currIndex] with
            | '[' -> findPair (currIndex + 1) currIndex targetDepth (leftCount + 1) chars
            | ']' ->
                if (leftCount - 1) = targetDepth then Some(lastStartIndex, currIndex)
                else findPair (currIndex + 1) lastStartIndex targetDepth (leftCount - 1) chars
            | _ -> findPair (currIndex + 1) lastStartIndex targetDepth leftCount chars

    let t = n.ToString()
    let range = findPair 0 -1 4 0 (t.ToCharArray())
    match range with
    | Some(i,i') -> 
        let (pairLeft, pairRight) = extractPair t i i'

        let t' = t.Remove(i, i' - i + 1).Insert(i, "0")
        let chars' = t'.ToCharArray()

        let leftSiblingRange = findNextNumber (i-1) -1 chars'
        let rightSiblingRange = findNextNumber (i+1) 1 chars'

        let applyRight =
            match rightSiblingRange with
            | Some(startIndex, endIndex) -> 
                let rightSibling = Int32.Parse(t'[startIndex..endIndex])
                let newValue = rightSibling + pairRight
                t'.Remove(startIndex, rightSibling.ToString().Length).Insert(startIndex, newValue.ToString())
            | None -> t'

        let applyLeft =
            match leftSiblingRange with
            | Some(startIndex, endIndex) -> 
                let leftSibling = Int32.Parse(t'[startIndex..endIndex])
                let newValue = leftSibling + pairLeft
                applyRight.Remove(startIndex, leftSibling.ToString().Length).Insert(startIndex, newValue.ToString())
            | None -> applyRight

        Some(parse applyLeft)
    | None -> None

let (|Split|_|) (n: Element)  =
    let isEven x = (x % 2) = 0
    let isOdd x = isEven x = false

    let mutable foundMatch = false

    let rec split n =
        if foundMatch then n
        else
            match n with
            | Constant(c) ->
                if c < 10 then n
                else
                    //printfn $"Constant {c} splits!"
                    foundMatch <- true
                    if isOdd c then
                        let t = (c + 1) / 2
                        Pair(Constant(t - 1), Constant(t))
                    else
                        let t = c / 2
                        Pair(Constant(t), Constant(t))
            | Pair(e1, e2) -> Pair(split e1, split e2)
    let r = split n
    if foundMatch then Some(r)
    else None

let total elements =
    let rec reduce (n: Element) =
        match n with
        | Explode(m) | Split(m) -> reduce(m)
        | _ -> n
    let add (n1: Element) (n2: Element) = reduce (Pair (n1, n2))
    Array.reduce add elements

let rec magitude element =
    match element with
    | Constant(c) -> c
    | Pair(e1, e2) -> (3 * (magitude e1)) + (2 * (magitude e2))

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
    "[[[[4,2],2],6],[8,7]]"; |]
           |> Array.map (fun s -> parse s)

printfn $"{total samples}" // [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]

let sampleHomework = [|
    "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]";
    "[[[5,[2,8]],4],[5,[[9,9],0]]]";
    "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]";
    "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]";
    "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]";
    "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]";
    "[[[[5,4],[7,7]],8],[[8,3],8]]";
    "[[9,3],[[9,9],[6,[4,9]]]]";
    "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]";
    "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"; |] |> Array.map parse

let sampleHomeworkSum = total sampleHomework
printfn $"{sampleHomeworkSum}"
printfn $"{magitude sampleHomeworkSum}" // 4140

// Puzzle data

let homework = System.IO.File.ReadAllLines "input.txt" |> Array.map parse
let homeworkSum = total homework
printfn $"{homeworkSum}"
printfn $"{magitude homeworkSum}" // 4243

// --- Part Two ---

let permute elements =
    let count = elements |> Array.length
    seq {
        for i = 0 to (count - 1) do
            for j = 0 to (count - 1) do
                if i <> j then yield [| elements[i]; elements[j] |]
    }

let findGreatestPair elements =
    permute elements
    |> Seq.map (fun elements -> magitude (total elements))
    |> Seq.max

// Sample data
printfn $"{findGreatestPair sampleHomework}" // 3993

// Puzzle data
printfn $"{findGreatestPair homework}" // 4701
