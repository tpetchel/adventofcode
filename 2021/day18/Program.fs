// https://adventofcode.com/2021/day/18
open System
open System.Text.RegularExpressions

// --- Day 18: Snailfish ---

type Element =
    | Constant of uint64
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

let isDigit (ch: char) =
   let mutable intvalue = 0UL
   //let ch = ch.ToString()
   System.UInt64.TryParse(ch.ToString(), &intvalue)

let (|ConstExpr|_|) (str: string) =
    let mutable intvalue = 0UL
    let digits = str |> Seq.takeWhile isDigit |> String.Concat
    if System.UInt64.TryParse(digits, &intvalue) then Some(digits, str.Substring(digits.Length))
    else None
    
    // let ch2 = str[0..1]
    // if System.Int32.TryParse(ch2, &intvalue) then Some(ch2, str.Substring(2))
    // else
    //     let ch = str[0].ToString()
    //     if System.Int32.TryParse(ch, &intvalue) then Some(ch, str.Substring(1))
    //     else None

let rec (|PairExpr|_|) (s: string) =
    match s with
    // [const,const]
    | TL "[" (ConstExpr(n1, (TL "," (ConstExpr(n2, (TL "]" rest)))))) -> 
        Some ((Constant(UInt64.Parse(n1)), Constant(UInt64.Parse(n2))), rest)
    // [pair,pair]
    | TL "[" (PairExpr(n1, (TL "," (PairExpr(n2, (TL "]" rest)))))) -> 
        Some ((Pair(n1), Pair(n2)), rest)
    // [const,pair]
    | TL "[" (ConstExpr(n1, (TL "," (PairExpr(n2, (TL "]" rest)))))) -> 
        Some ((Constant(UInt64.Parse(n1)), Pair(n2)), rest)
    // [pair,const]
    | TL "[" (PairExpr(n1, (TL "," (ConstExpr(n2, (TL "]" rest)))))) -> 
        Some ((Pair(n1), Constant(UInt64.Parse(n2))), rest)
    | _ -> None

let parse s =
    match s with
    | PairExpr(p, _) -> Pair(p)
    | _ -> failwith "Syntax error"

                        // The pair's left value is added to the first regular number to the left
                        // of the exploding pair (if any).
                        // The pair's right value is added to the first regular number to the right
                        // of the exploding pair (if any).
                        // Exploding pairs will always consist of two regular numbers.
                        // Then, the entire exploding pair is replaced with the regular number 0.

type MatchType =
    | None = 0
    | Left = 1
    | Right = 2



let (|Explode|_|) (n: Element) =
    let extractPair (t: string) i i' =
        let t' = t[i..i']
        let matched = Regex.Match(t', "\[(\d+)\,(\d+)\]")
        let pairLeft = UInt64.Parse(matched.Groups.[1].Value)
        let pairRight = UInt64.Parse(matched.Groups.[2].Value)
        (pairLeft, pairRight)
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
            let i1 = min startIndex endIndex
            let i2 = max startIndex endIndex
            Some(i1, i2)
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
                let rightSibling = UInt64.Parse(t'[startIndex..endIndex])
                let newValue = rightSibling + pairRight
                t'.Remove(startIndex, rightSibling.ToString().Length).Insert(startIndex, newValue.ToString())
                //let chars' = t'.ToCharArray()
                //chars'[n] <- newValue.ToString()[0]
                //new string(chars')
            | None -> t'

        let applyLeft =
            match leftSiblingRange with
            | Some(startIndex, endIndex) -> 
                let leftSibling = UInt64.Parse(t'[startIndex..endIndex])
                let newValue = leftSibling + pairLeft
                applyRight.Remove(startIndex, leftSibling.ToString().Length).Insert(startIndex, newValue.ToString())
                // let chars' = applyRight.ToCharArray()
                // chars'[n] <- newValue.ToString()[0]
                // new string(chars')
            | None -> applyRight

        Some(parse applyLeft)
    | None -> None

    // I gave up trying to process this as a graph, as it's very difficult
    // to access a node's lexical siblings, let alone update them cleanly.
    // So here, we basically serialize the object to a string, update the string
    // directly by using regular expressions, and parse it back out.
    // Here are the four patterns:
    // [[[[[9,8],1],2],3],4] (no regular number to the left of [9,8])
    // [7,[6,[5,[4,[3,2]]]]] (no regular number to the right of [3,2])
    // [[6,[5,[4,[3,2]]]],1] (nest left)
    // [1,[[[[9,8],1],2],3]] (nest right)
    
    

    // let apply (s: string) (matchType: MatchType) =
    //     let t = n.ToString()
    //     match matchType with
    //     | MatchType.Left ->
    //         let index = t.IndexOf("[[[[" + s)
    //         let beforeMatch = t[index..0]
    //         let leftSiblingIndex = beforeMatch |> Seq.tryFindIndexBack (fun ch -> isInt ch)

    //         let index = t.IndexOf("[[[[" + s) + 4 + s.Length
    //         let afterMatch = t[index..]
    //         let rightSiblingIndex = afterMatch |> Seq.tryFindIndex (fun ch -> isInt ch)
            
    //         let matched = Regex.Match(s, "\[(\d)\,(\d)\]")
    //         let pairLeft = Int32.Parse(matched.Groups.[1].Value)
    //         let pairRight = Int32.Parse(matched.Groups.[2].Value)

    //         let applyLeft =
    //             match leftSiblingIndex with
    //             | Some(n) -> 
    //                 let leftSibling = Int32.Parse(beforeMatch[n].ToString())
    //                 let newValue = leftSibling + pairLeft
    //                 let arr = beforeMatch.ToCharArray()
    //                 arr[n] <- newValue.ToString()[0]
    //                 new string(arr)
    //             | None -> beforeMatch
    //         let applyRight =
    //             match rightSiblingIndex with
    //             | Some(n) -> 
    //                 let rightSibling = Int32.Parse(afterMatch[n].ToString())
    //                 let newValue = rightSibling + pairRight
    //                 let arr = afterMatch.ToCharArray()
    //                 arr[n] <- newValue.ToString()[0]
    //                 new string(arr)
    //             | None -> afterMatch
    //         Some(parse $"{applyLeft}[[[0{applyRight}")
    //     | MatchType.Right ->
    //         let index = t.IndexOf(s + "]]]]")
    //         let beforeMatch = t[index..]
    //         let leftSiblingIndex = beforeMatch |> Seq.tryFindIndexBack (fun ch -> isInt ch)

    //         let index = t.IndexOf("[[[[" + s) + 4 + s.Length
    //         let afterMatch = t[index..]
    //         let rightSiblingIndex = afterMatch |> Seq.tryFindIndex (fun ch -> isInt ch)
            
    //         let matched = Regex.Match(s, "\[(\d)\,(\d)\]")
    //         let pairLeft = Int32.Parse(matched.Groups.[1].Value)
    //         let pairRight = Int32.Parse(matched.Groups.[2].Value)

    //         let applyLeft =
    //             match leftSiblingIndex with
    //             | Some(n) -> 
    //                 let leftSibling = Int32.Parse(beforeMatch[n].ToString())
    //                 let newValue = leftSibling + pairLeft
    //                 let arr = beforeMatch.ToCharArray()
    //                 arr[n] <- newValue.ToString()[0]
    //                 new string(arr)
    //             | None -> beforeMatch
    //         let applyRight =
    //             match rightSiblingIndex with
    //             | Some(n) -> 
    //                 let rightSibling = Int32.Parse(afterMatch[n].ToString())
    //                 let newValue = rightSibling + pairRight
    //                 let arr = afterMatch.ToCharArray()
    //                 arr[n] <- newValue.ToString()[0]
    //                 new string(arr)
    //             | None -> afterMatch
    //         Some(parse $"{applyLeft}[[[0{applyRight}")
    //     | _ -> failwith "Invalid match type"

// [[[[0,7],4],[7,[[8,4],9]]],[1,1]]
// [    ,7],4],[7,[[8,4],9]]],[1,1]]

        // let patterns = [|
        //     ("^(\[{4,})\[(\d)\,(\d)\]\,(\d)", (fun (matched: Match) -> 
        //         let entireMatch = matched.Groups.[0].Value
        //         let leftBraces = matched.Groups.[1].Value
        //         let e1 = Int32.Parse(matched.Groups.[2].Value)
        //         let e2 = Int32.Parse(matched.Groups.[3].Value)
        //         let right = Int32.Parse(matched.Groups.[4].Value)
        //         s.Replace(entireMatch, $"{leftBraces}0,{e2 + right}")));
        //     ("(\d)\,\[(\d)\,(\d)\](\[{4,})$", (fun (matched: Match) -> 
        //         let entireMatch = matched.Groups.[0].Value
        //         let left = Int32.Parse(matched.Groups.[1].Value)
        //         let e1 = Int32.Parse(matched.Groups.[2].Value)
        //         let e2 = Int32.Parse(matched.Groups.[3].Value)
        //         let rightBraces = matched.Groups.[4].Value
        //         s.Replace(entireMatch, $"{e1 + left},0{rightBraces}")))
        // |]

        // let matches = 
        //     patterns 
        //     |> Array.map (fun (pattern, apply) -> (Regex.Match(s, pattern), apply))
        //     |> Array.choose (fun (matched, apply) -> 
        //         if matched.Success then Some(matched, apply); else None)
        // if (matches |> Array.isEmpty) then None
        // else 
        //     let (matched, apply) = matches |> Array.minBy (fun (matched : Match, _) -> matched.Index)
        //     Some(parse (apply matched))

    // let getMatchType lbraces rbraces =
    //     if lbraces > rbraces then MatchType.Left
    //     else MatchType.Right

    // let scan (n: Element) = 
    //     let mutable matchType = MatchType.None
    //     let rec explode n nestLevel lbraces rbraces =
    //         //printfn $"{nestLevel} -> {n}"
    //         if matchType <> MatchType.None then (n, false)
    //         else
    //             match n with
    //             | Constant(_) -> (n, false)
    //             | Pair(e1, e2) -> 
    //                 if nestLevel = 4 then
    //                     printfn $"Pair {n} explodes!"
    //                     matchType <- getMatchType lbraces rbraces
    //                     (Pair(e1, e2), true)
    //                 else
    //                     let nestLevel = nestLevel + 1
    //                     match explode e1 nestLevel (1 + lbraces) rbraces with
    //                     | (n, true) -> (n, true)
    //                     | (n, false) ->
    //                         match explode e2 nestLevel lbraces (1 + rbraces) with
    //                         | (m, true) -> (m, true)
    //                         | (m, false) -> (m, false)
    //     let (r, _) = explode n 0 0 0
    //     if matchType <> MatchType.None then Some(r.ToString(), matchType)
    //     else None

    // let r = scan n
    // match r with 
    // | Some(m, isLeftMatch) -> apply m isLeftMatch
    // | None -> None
    // let mutable foundMatch = false
    // let rec explode n (leftSibling, rightSibling) nestLevel =
    //     //printfn $"{nestLevel} -> {n}"
    //     if foundMatch then (n, false)
    //     else
    //         match n with
    //         | Constant(_) -> (n, false)
    //         | Pair(e1, e2) -> 
    //             if nestLevel = 4 then
    //                 printfn $"Pair {n} explodes!"
    //                 foundMatch <- true

    //                 // The pair's left value is added to the first regular number to the left
    //                 // of the exploding pair (if any).
    //                 // The pair's right value is added to the first regular number to the right
    //                 // of the exploding pair (if any).
    //                 // Exploding pairs will always consist of two regular numbers.
    //                 // Then, the entire exploding pair is replaced with the regular number 0.
    //                 let c1 = constantValue e1
    //                 let c2 = constantValue e2

    //                 match leftSibling with
    //                 | Some(Constant c) -> (Pair(Constant(c + c1), Constant(0)), true)
    //                 | _ -> 
    //                     match rightSibling with
    //                     | Some(Constant c) -> (Pair(Constant(0), Constant(c + c2)), true)
    //                     | _ -> failwith "Expected left or right Constant sibling"
    //             else
    //                 let nestLevel = nestLevel + 1
    //                 match explode e1 (None, Some(e2)) nestLevel with
    //                 | (n, true) -> (n, false)
    //                 | (n, false) ->
    //                     match explode e2 (Some(e1), None) nestLevel with
    //                     | (m, true) -> (m, false)
    //                     | (m, false) -> (Pair(n, m), false)
    // let (r, _) = explode n (None, None) 0
    // if foundMatch then Some(r)
    // else None

let (|Split|_|) (n: Element)  =
    // Even numbers are divisible by 2.
    let isEven x = (x % 2UL) = 0UL
    // Odd numbers are not even.
    let isOdd x = isEven x = false

    let mutable foundMatch = false
    let rec split n =
        if foundMatch then n
        else
            match n with
            | Constant(c) ->
                if c < 10UL then n
                else
                    //printfn $"Constant {c} splits!"
                    foundMatch <- true
                    if isOdd c then
                        let t = (c + 1UL) / 2UL
                        Pair(Constant(t - 1UL), Constant(t))
                    else
                        let t = c / 2UL
                        Pair(Constant(t), Constant(t))
            | Pair(e1, e2) ->
                let r1 = split e1
                let r2 = split e2
                Pair(r1, r2)
                // match split e1 with
                //     | (n, true) -> (Pair(n, e2), false)
                //     | (n, false) ->
                //         match split e2 with
                //         | (m, true) -> (Pair(e1, m), false)
                //         | (m, false) -> (Pair(n, m), false)
    let r = split n
    if foundMatch then Some(r)
    else None

let total elements =
    let rec reduce (n: Element) =
        match n with
        | Explode(m) ->
            //printfn $"After explode: {m}"
            reduce(m)
        | Split(m) ->
            //printfn $"After split: {m}"
            reduce(m)
        | _ -> n
    let add (n1: Element) (n2: Element) =
        //printfn $"  {n1}"
        //printfn $"+ {n2}"
        reduce (Pair (n1, n2))
    Array.reduce add elements

let rec magitude element =
    match element with
    | Constant(c) -> c
    | Pair(e1, e2) -> (3UL * (magitude e1)) + (2UL * (magitude e2))

// --- Part One ---

// Sample data

// printfn "==="
// let t = parse "[[[[[9,8],1],2],3],4]"
// let _ = total [| t |]
// printfn $"{t}"

// match t with
// | Explode(t') | Split(t') -> printfn $"{t'}"
// | _ -> printfn "None"


// printfn "==="
// let t' = parse "[7,[6,[5,[4,[3,2]]]]]"
// let _ = total [| t' |]
// printfn $"{t'}"

// match t' with
// | Explode(t'') | Split(t'') -> printfn $"{t''}"
// | _ -> printfn "None"


// printfn "==="
// let t'' = parse "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
// let _ = total [| t'' |]
// printfn $"{t''}"

// match t'' with
// | Explode(t''') | Split(t''') -> printfn $"{t'''}"
// | _ -> printfn "None"

printfn "==="

// let x = parse "[[[[4,3],4],4],[7,[[8,4],9]]]"
// let y = parse "[1,1]"
// let z = total [| x; y |]
// printfn $"{z}"

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

let sum = total samples
printfn $"{sum}" // [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]

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

let sum' = total sampleHomework
printfn $"{sum'}"
let mag = magitude sum'
printfn $"{mag}" // 4140

let homework = System.IO.File.ReadAllLines "input.txt" |> Array.map parse
let sum'' = total homework
printfn $"{sum''}"
let mag' = magitude sum''
printfn $"{mag'}" // 4243


// [[[[0,7],4],[7,[6,7]]],[1,1]]
// [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
// Puzzle data

// --- Part Two ---

let permute elements =
    let count = elements |> Array.length
    seq {
        for i = 0 to (count - 1) do
            for j = 0 to (count - 1) do
                if i <> j then yield [| elements[i]; elements[j] |]
    }
 
// Sample data

//let permutations = sampleHomework |> Array.toList |> permute
let permutations = permute sampleHomework
let mags = permutations |> Seq.map (fun elements -> 
    (elements, magitude (total elements)))
let (_, maxMag) = mags |> Seq.maxBy (fun (_, mag) -> mag)
printfn $"{maxMag}" // 3993


// Puzzle data
let permutations' = permute homework
let mags' = permutations' |> Seq.map (fun elements -> 
    (elements, magitude (total elements)))
let (_, maxMag') = mags' |> Seq.maxBy (fun (_, mag) -> mag)
printfn $"{maxMag'}" // 4701