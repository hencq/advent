open System.IO

let test = [
  "2199943210"
  "3987894921"
  "9856789892"
  "8767896789"
  "9899965678" ]

let input = File.ReadAllLines "input9.txt"

type HMap = int array array

let readHeights (lines : string seq) =
  lines
  |> Seq.map (fun l ->
              l
              |> Array.ofSeq
              |> Array.map (fun c -> int c - int '0'))
  |> Array.ofSeq

type Point = int * int
let getPoint hmap (x, y) =
  hmap
  |> Array.tryItem y
  |> Option.bind (fun row -> Array.tryItem x row)

let neighbors (x, y) =
  [(x-1, y); (x+1, y); (x, y-1); (x, y+1)]

let findLowPoints (hmap : HMap) =
  hmap
  |> Array.indexed
  |> Array.collect (fun (y, row) ->
                    row
                    |> Array.mapi (fun x v ->
                                    let ns =
                                       neighbors (x, y)
                                       |> Seq.map (getPoint hmap)
                                       |> Seq.choose id
                                    if (Seq.forall ((<) v) ns) then
                                      Some (x, y)
                                    else None))
  |> Seq.choose id


let part1 input =
  let hmap = input |> readHeights
  hmap
  |> findLowPoints
  |> Seq.map (getPoint hmap)
  |> Seq.choose id
  |> Seq.map ((+) 1)
  |> Seq.sum
  
part1 input

let rec findBasin hmap seen pt =
  let getPoint = getPoint hmap
  let tovisit =
    neighbors pt
    |> List.filter (fun p -> not (Set.contains p seen))
    |> List.filter (fun p ->
                   match getPoint p with
                   | Some v -> v < 9
                   | None -> false)
  (Set.add pt seen, tovisit)
  ||> List.fold (findBasin hmap)

let part2 input =
  let hmap = readHeights input
  hmap
  |> findLowPoints
  |> Seq.map (findBasin hmap Set.empty)
  |> Seq.map Set.count
  |> Seq.sortDescending
  |> Seq.take 3
  |> Seq.fold (*) 1

part2 input
