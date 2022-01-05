open System.IO

type Image =
  { Algo : bool[]
    Data : bool[,]
    Default : bool }
    
let get (x, y) (img : Image) =
  let width = Array2D.length2 img.Data
  let height = Array2D.length1 img.Data
  if x < 0 || y < 0 then img.Default
  elif x >= width || y >= height then img.Default
  else img.Data[y, x]

let readInput input =
  let [| algs; ms |] = File.ReadAllText input |> fun txt -> txt.Split("\n\n")
  let alg =
    algs.Replace("\n", "").ToCharArray()
    |> Array.map (function
                  | '#' -> true
                  | _ -> false)
  let img =
    ms.Trim().Split("\n")
    |> Seq.map (fun l ->
                      l
                      |> Seq.map (fun ch ->
                                     match ch with
                                     | '#' -> true
                                     | _ -> false))
    |> array2D
  { Algo = alg; Data = img; Default = false }
    
let test = readInput "test20.txt"

let ptVal img (x, y) =
  [ for y' in y - 1 .. y + 1 do
      for x' in x - 1 .. x + 1 ->
        (x', y') ]
  |> List.map (fun pt -> if get pt img then 1 else 0)
  |> List.reduce (fun sum n -> 2 * sum + n)  

let step img =
  let height = (Array2D.length1 img.Data) + 2
  let width = (Array2D.length2 img.Data) + 2
  let data = Array2D.create height width false
  for y in 0 .. height - 1 do
    for x in 0 .. width - 1 do
      data[y, x] <- img.Algo[ptVal img (x - 1, y - 1)]
  
  let deft = if img.Default then img.Algo[511] else img.Algo[0]
  { img with Data = data; Default = deft }
  
let printImg img =
  let height = Array2D.length1 img.Data
  let width = Array2D.length2 img.Data
  printfn ""
  for y in 0 .. height - 1 do
    for x in 0 .. width - 1 do
      if img.Data[y, x] then
        printf "#"
      else
        printf "."
    printfn ""
    
let litCount img =
  let mutable count = 0
  img.Data
  |> Array2D.iter (fun v -> if v then count <- count + 1)
  count

let part1 img =
  img
  |> step
  |> step
  |> litCount

let part2 img =
  seq { for i in 1 .. 50 -> i }
  |> Seq.fold (fun img _ -> step img) img
  |> litCount

part1 test
part2 test

let input = readInput "input20.txt"
part1 input
part2 input





