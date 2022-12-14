open System
open System.IO

let read fname =
    File.ReadAllText fname
    |> fun x -> x.Split("\n\n")
    |> Array.map (fun x -> x.Split("\n") |> Array.filter (fun x -> x <> "") |> Array.map int)

let elves = read "input1.txt"

let part1 =
    elves
    |> Array.map Array.sum
    |> Array.max

let part2 =
    elves
    |> Array.map Array.sum
    |> Array.sortDescending
    |> Array.take 3
    |> Array.sum
