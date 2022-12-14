open System
open System.IO

type Hand = Rock | Paper | Scissors

let readLine (l : string) =
    let p1 = match l[0] with
        | 'A' -> Rock
        | 'B' -> Paper
        | _  -> Scissors
    let p2 = match l[2] with
        | 'X' -> Rock
        | 'Y' -> Paper
        | _ -> Scissors
    (p1, p2)

let hands =
    File.ReadAllLines "input2.txt"
    |> Array.map readLine

let score1 hand =
    match hand with
        | (Rock, Rock) -> 3 + 1
        | (Rock, Paper) -> 6 + 2
        | (Rock, Scissors) -> 0 + 3
        | (Paper, Rock) -> 0 + 1
        | (Paper, Paper) -> 3 + 2
        | (Paper, Scissors) -> 6 + 3
        | (Scissors, Rock) -> 6 + 1
        | (Scissors, Paper) -> 0 + 2
        | (Scissors, Scissors) -> 3 + 3

hands
|> Array.map score1
|> Array.sum

type Outcome = Win | Lose | Draw

let score2 hand =
    let meaning2 hand =
        match hand with
            | hand, Rock -> (hand, Lose)
            | hand, Paper -> (hand, Draw)
            | hand, Scissors -> (hand, Win)
    match meaning2 hand with
        | Rock, Win -> 6 + 2
        | Rock, Lose -> 0 + 3
        | Rock, Draw -> 3 + 1
        | Paper, Win -> 6 + 3
        | Paper, Lose -> 0 + 1
        | Paper, Draw -> 3 + 2
        | Scissors, Win -> 6 + 1
        | Scissors, Lose -> 0 + 2
        | Scissors, Draw -> 3 + 3

let part2 =
    hands
    |> Array.map score2
    |> Array.sum
