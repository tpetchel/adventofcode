// https://adventofcode.com/2021/day/10

// --- Day 10: Syntax Scoring ---

// --- Part One ---

let readInput filename = System.IO.File.ReadAllLines(filename)

let TokenPairs = Map [ ('(', ')'); ('[', ']'); '<', '>'; '{', '}'; ]
let isStartToken token = TokenPairs |> Map.containsKey token
let areBalanced token1 token2 = TokenPairs[token1] = token2

let checkSyntax (lines : string array) =
    let mutable ErrorTable = Map.empty
    let rec balanceTokens (tokens : char list) (context : char list) =
        match tokens with
        | currToken :: remainingTokens -> 
            if isStartToken currToken then 
                // Push current token onto context stack and balance
                // what's left
                balanceTokens (remainingTokens) (currToken :: context)
            else
                match context with
                // Verify whether current token balances what's on top of
                // the context stack 
                | currContext :: remainingContexts -> 
                    if areBalanced currContext currToken then
                        balanceTokens (remainingTokens) (remainingContexts)
                    else 
                        // They're not balanced; stop
                        let expected = TokenPairs[currContext]
                        let found = currToken
                        printfn "Expected %c, but found %c instead" expected found
                        Some(expected, found)
                // syntax is incomplete
                | _ -> None
        | _ -> None
    let isCorrupted (line : string) =
        let error = balanceTokens (line.ToCharArray() |> Array.toList) []
        match error with 
        | Some(expected, found) -> 
            ErrorTable <- ErrorTable |> Map.add line (expected, found)
            true
        | None -> false
    let (corrupted, remaining) = lines |> Array.partition isCorrupted
    (corrupted, remaining, ErrorTable)

let ErrorScores = Map [ (')', 3); (']', 57); '>', 1197; '}', 25137; ]

// Validate sample input first
let sampleInput = readInput "sample.txt"
let (corrupted, incomplete, errors) = checkSyntax sampleInput
corrupted |> Array.iter(fun line -> printfn "%s" line)
let errorScore = corrupted |> Array.sumBy(fun line ->
    let (_, found) = errors[line]
    ErrorScores[found])
printfn "%i" errorScore // 26397

// Now validate puzzle input
let puzzleInput = readInput "input.txt"
let (corrupted', incomplete', errors') = checkSyntax puzzleInput
let errorScore' = corrupted' |> Array.sumBy(fun line ->
    let (_, found) = errors'[line]
    ErrorScores[found])
printfn "%i" errorScore' // 316851

// --- Part Two ---

// In hindsight, could've combined with checkSyntax. Although, things got 
// complicated there and a combined solution would need refactoring.

let autocomplete (line : string) =
    let rec balanceTokens (tokens : char list) (context : char list) =
        match tokens with
        | currToken :: remainingTokens -> 
            if isStartToken currToken then 
                // Push current token onto context stack and balance
                // what's left
                balanceTokens (remainingTokens) (currToken :: context)
            else
                match context with
                // Verify whether current token balances what's on top of
                // the context stack 
                | _ :: remainingContexts -> 
                    balanceTokens (remainingTokens) (remainingContexts)
                | _ -> failwith "Unexpected: empty context stack"
        // syntax is incomplete; map remaining to their closing tokens
        | _ -> context |> List.map(fun token -> TokenPairs[token])
    balanceTokens (line.ToCharArray() |> Array.toList) []

let scoreAutocomplete (line : string) =
    let Scores = Map [ (')', 1UL); (']', 2UL); '}', 3UL; '>', 4UL; ]
    let score (ch : char) (acc : uint64) = (acc * 5UL)
    line.ToCharArray() 
        |> Array.map(fun ch -> Scores[ch]) 
        |> Array.fold(fun acc score -> (acc * 5UL) + score) 0UL

// Validate sample input first
let complete = incomplete |> Array.map autocomplete
complete |> Array.iter(fun s -> printfn "%s" (s |> System.String.Concat))
let scores = complete |> Array.map(fun s -> s |> System.String.Concat |> scoreAutocomplete)
// Sort and take middle score
let answer = scores |> Array.sort |> Array.skip (scores.Length / 2) |> Array.take 1 |> Array.exactlyOne
printfn "%i" answer // 288957

// Now validate puzzle input
let complete' = incomplete' |> Array.map autocomplete
complete' |> Array.iter(fun s -> printfn "%s" (s |> System.String.Concat))
let scores' = complete' |> Array.map(fun s -> s |> System.String.Concat |> scoreAutocomplete)
// Sort and take middle score
let answer' = scores' |> Array.sort |> Array.skip (scores'.Length / 2) |> Array.take 1 |> Array.exactlyOne
printfn "%i" answer' // 2182912364