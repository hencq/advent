open System.IO

//let test = File.ReadAllLines "test1.txt" |> Seq.map int
let input =
  File.ReadAllLines "input1.txt" |> Seq.map int

let increases depths =
  Seq.pairwise depths
  |> Seq.filter (fun (a, b) -> a < b)
  |> Seq.length

let window (depths: int seq) =
  Seq.windowed 3 depths |> Seq.map Array.sum

input |> increases
input |> window |> increases
