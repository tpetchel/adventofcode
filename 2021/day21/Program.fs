// https://adventofcode.com/2021/day/21

// --- Day 21: Dirac Dice ---

// This one was pretty easy!

let nextPlayer player =
    if player = 0 then 1
    else 0

let moveSpaces position score = 
    let newPosition = position + score - (score/10*10)
    if newPosition > 10 then newPosition - 10
    else newPosition

let playDeterministicDice (positions: int[]) rollsPerMove = 
    let roll die count = 
        let nextFace face =
            if face > 100 then face - 100 
            else face
        seq {
            for i = 0 to (count-1) do 
                yield nextFace (die + i)
        } |> Seq.toArray

    let computeScore (rolls: int[]) = rolls |> Array.sum
    let scores = Array.zeroCreate (positions.Length)
    let rec nextMove rollCount currPlayer die = 
        let rolls = roll die rollsPerMove
        let score = computeScore rolls
        positions[currPlayer] <- moveSpaces positions[currPlayer] score
        scores[currPlayer] <- scores[currPlayer] + positions[currPlayer]

        printfn "Player %i rolls %A and moves to space %i for a total score of %i." (currPlayer+1) rolls positions[currPlayer] scores[currPlayer]

        if scores[currPlayer] >= 1000 then
            scores, currPlayer, (rollCount + rollsPerMove)
        else
            nextMove (rollCount + rollsPerMove) (nextPlayer currPlayer) ((rolls |> Array.last) + 1)
    nextMove 0 0 1

// --- Part One ---

// Sample data
let finalScores, winner, rolls = playDeterministicDice [| 4; 8; |] 3
let losingScore = finalScores[nextPlayer winner]
printfn $"{losingScore}" // 745
printfn $"{rolls}" // 993
printfn $"{losingScore * rolls}" // 739785

// Puzzle data
let finalScores', winner', rolls' = playDeterministicDice [| 9; 6; |] 3
let losingScore' = finalScores'[nextPlayer winner']
printfn $"{losingScore'}" // 915
printfn $"{rolls'}" // 1098
printfn $"{losingScore' * rolls'}" // 1004670

// --- Part Two ---

// The insight here is to precompute the *number of ways* to 
// arrive at each score. For example, 
// [1,2,2]
// [2,2,1]
// Both give you a score of 5. There's no need to run both; simply
// multiply the number of combinations by that score.

for a in [|1;2;3|] do
    for b in [|1;2;3|] do
        for c in [|1;2;3|] do
            printfn $"{a} {b} {c} -> {a+b+c}"

// The above loop prints this:

(*
1 1 1 -> 3
1 1 2 -> 4
1 1 3 -> 5
1 2 1 -> 4
1 2 2 -> 5
1 2 3 -> 6
1 3 1 -> 5
1 3 2 -> 6
1 3 3 -> 7
2 1 1 -> 4
2 1 2 -> 5
2 1 3 -> 6
2 2 1 -> 5
2 2 2 -> 6
2 2 3 -> 7
2 3 1 -> 6
2 3 2 -> 7
2 3 3 -> 8
3 1 1 -> 5
3 1 2 -> 6
3 1 3 -> 7
3 2 1 -> 6
3 2 2 -> 7
3 2 3 -> 8
3 3 1 -> 7
3 3 2 -> 8
3 3 3 -> 9
*)

// Which gives us this mapping of combinations to counts:

(*
1 1 1 -> 3

1 1 2 -> 4
1 2 1 -> 4
2 1 1 -> 4

1 1 3 -> 5
1 2 2 -> 5
1 3 1 -> 5
2 1 2 -> 5
2 2 1 -> 5
3 1 1 -> 5

1 2 3 -> 6
1 3 2 -> 6
2 1 3 -> 6
2 2 2 -> 6
2 3 1 -> 6
3 1 2 -> 6
3 2 1 -> 6

1 3 3 -> 7
2 2 3 -> 7
2 3 2 -> 7
3 1 3 -> 7
3 2 2 -> 7
3 3 1 -> 7

2 3 3 -> 8
3 2 3 -> 8
3 3 2 -> 8

3 3 3 -> 9
*)

// In terms of a map:

let scoreCounts = Map [
    (3, 1UL);  // 1 way to score a 3 (1,1,1)
    (4, 3UL);  // 3 ways to score a 4
    (5, 6UL);  // and so on...
    (6, 7UL);
    (7, 6UL);
    (8, 3UL);
    (9, 1UL); ] // 1 way to score a 9 (3,3,3)

let playDiracDice (positions: int[]) = 
    let winCounts = Array.create positions.Length 0UL
    let rec nextMoves currPlayer positions scores depth = 
        scoreCounts |> Map.iter (fun score count ->
            let positions' = positions |> Array.copy
            let scores' = scores |> Array.copy
            positions'[currPlayer] <- moveSpaces positions'[currPlayer] score
            scores'[currPlayer] <- scores'[currPlayer] + positions'[currPlayer]
            if scores'[currPlayer] >= 21 then
                winCounts[currPlayer] <- winCounts[currPlayer] + (count * depth)
            else
                nextMoves (nextPlayer currPlayer) positions' scores' (depth * count))
    nextMoves 0 positions (Array.zeroCreate positions.Length) 1UL
    winCounts

// Sample data
let winCounts = playDiracDice [| 4; 8; |]
printfn "%A" winCounts // [|444356092776315UL; 341960390180808UL|]
printfn "%i" (winCounts |> Array.max) // 444356092776315

// Puzzle data
let winCounts' = playDiracDice [| 9; 6; |]
printfn "%A" winCounts' // [|492043106122795UL; 267086464416104UL|]
printfn "%i" (winCounts' |> Array.max) // 492043106122795
