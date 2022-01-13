// https://adventofcode.com/2021/day/21

// --- Day 21: Dirac Dice ---

let nextPlayer player =
    if player = 0 then 1
    else 0

let moveSpaces position score = 
    let newPosition = position + score - (score/10*10)
    if newPosition > 10 then newPosition - 10
    else newPosition

let playDeterministic (positions: int[]) rollsPerMove = 
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
let finalScores, winner, rolls = playDeterministic [| 4; 8; |] 3
let losingScore = finalScores[nextPlayer winner]
printfn $"{losingScore}" // 745
printfn $"{rolls}" // 993
printfn $"{losingScore * rolls}" // 739785

// Puzzle data
let finalScores', winner', rolls' = playDeterministic [| 9; 6; |] 3
let losingScore' = finalScores'[nextPlayer winner']
printfn $"{losingScore'}" // 915
printfn $"{rolls'}" // 1098
printfn $"{losingScore' * rolls'}" // 1004670

// --- Part Two ---

let playDirac (positions: int[]) = 
    let winCounts = Array.create positions.Length 0UL
    let rec nextMoves currPlayer positions scores = 
        for roll in [|1;2;3|] do
            let positions' = positions |> Array.copy
            let scores' = scores |> Array.copy
            positions'[currPlayer] <- moveSpaces positions'[currPlayer] roll
            scores'[currPlayer] <- scores'[currPlayer] + positions'[currPlayer]

            printfn "Player %i rolls %i and moves to space %i for a total score of %i." (currPlayer+1) roll positions'[currPlayer] scores'[currPlayer]

            if scores'[currPlayer] >= 21 then
                winCounts[currPlayer] <- winCounts[currPlayer] + 1UL
            else
                nextMoves (nextPlayer currPlayer) positions' scores'
    nextMoves 0 positions (Array.zeroCreate positions.Length)
    winCounts

let winCounts = playDirac [| 4; 8; |]
printfn "%A" winCounts
// 444_356_092_776_315
// 341_960_390_180_808