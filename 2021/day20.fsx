open System.IO

type Image =
  { Algo : bool[]
    Data : Map<int * int, bool>
    Default : bool }
    
let get pt (img : Image) =
  match Map.tryFind pt img.Data with
  | Some v -> v
  | None -> img.Default

let readInput input =
  let [| algs; ms |] = File.ReadAllText input |> fun txt -> txt.Split("\n\n")
  let alg =
    algs.Replace("\n", "").ToCharArray()
    |> Array.map (function
                  | '#' -> true
                  | _ -> false)
  let img =
    ms.Split("\n")
    |> Seq.indexed
    |> Seq.collect (fun (y, l) ->
                      l
                      |> Seq.mapi (fun x ch ->
                                     match ch with
                                     | '#' -> ((x, y), true)
                                     | _ -> ((x, y), false)))
    |> Map.ofSeq 
  { Algo = alg; Data = img; Default = false }
    
let test = readInput "test20.txt"

let dims img =
  let xs = img.Data |> Map.toSeq |> Seq.map (fst >> fst)
  let ys = img.Data |> Map.toSeq |> Seq.map (fst >> snd)
  (Seq.min xs, Seq.min ys, Seq.max xs, Seq.max ys)
  
let ptVal img (x, y) =
  [ for y' in y - 1 .. y + 1 do
      for x' in x - 1 .. x + 1 ->
        (x', y') ]
  |> List.map (fun pt -> if get pt img then 1 else 0)
  |> List.reduce (fun sum n -> 2 * sum + n)  

let step img =
  let x0, y0, x1, y1 = dims img
  [ for y in y0 - 1 .. y1 + 1 do
      for x in x0 - 1 .. x1 + 1 do
        (x, y) ]
  |> List.map (fun pt -> (pt, img.Algo[ptVal img pt]))
  |> Map.ofList
  |> fun data ->
       let deft = if img.Default then img.Algo[511] else img.Algo[0]
       { img with Data = data; Default = deft }
  
let printImg img =
  let x0, y0, x1, y1 = dims img
  printfn ""
  for y in y0 .. y1 do
    for x in x0 .. x1 do
      if img.Data[(x, y)] then
        printf "#"
      else
        printf "."
    printfn ""
    


let part1 img =
  img
  |> step
  |> step
  |> fun img -> img.Data |> Map.values |> Seq.filter id |> Seq.length

let part2 img =
  seq { for i in 1 .. 50 -> i }
  |> Seq.fold (fun img _ -> step img) img
  |> fun img -> img.Data |> Map.values |> Seq.filter id |> Seq.length

part1 test
part2 test

let input = readInput "input20.txt"
part1 input
part2 input




