open System.IO

let test = "16,1,2,0,4,2,7,1,2,14".Split(",") |> Array.map int
let input =
  File.ReadAllText "input7.txt"
  |> (fun txt -> txt.Split(","))
  |> Array.map int

let cost target crabs =
  crabs
  |> Seq.map (fun c -> abs (target - c))
  |> Seq.sum

let cost2 target crabs =
  crabs
  |> Seq.map (fun c ->
               let diff = abs (target - c)
               (diff + 1) * diff / 2)
  |> Seq.sum

let findBestHorizontal costfn crabs =
  let targets = seq {Seq.min crabs .. Seq.max crabs}
  targets
  |> Seq.map (fun t -> costfn t crabs)
  |> Seq.sort
  |> Seq.head

findBestHorizontal cost input
findBestHorizontal cost2 input
