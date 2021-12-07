open System.IO

let test = "3,4,3,1,2".Split(",") |> Array.toList |> List.map int
let input =
  File.ReadAllText "input6.txt"
  |> (fun x -> x.Split(","))
  |> Array.toList
  |> List.map int                                              

let sim n init =
  let state = Array.init 9 (fun _ -> bigint 0)
  init
  |> List.countBy id
  |> List.iter (fun (k, v) -> state.[k] <- (bigint v))

  let mutable spawn = bigint 0
  for i = 1 to n do
    spawn <- state.[0]
    for age = 0 to 8 do
      match age with
      | 8 -> state.[8] <- spawn
      | 6 -> state.[6] <- spawn + state.[7]
      | k -> state.[k] <- state.[k+1]
  Array.sum state

sim 80 input
sim 256 input
