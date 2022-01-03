open System.IO
open System.Text.RegularExpressions

let readInput fname =
  let lines = File.ReadAllLines fname |> List.ofArray
  let points =
    lines
    |> List.takeWhile (fun l -> l <> "")
    |> List.map (fun l -> l.Split(",") |> Array.map int)
    |> List.map (fun [|x; y|] -> (x, y))
    |> Set.ofList
  let folds =
    lines
    |> List.skipWhile (fun l -> l <> "")
    |> List.skip 1
    |> List.map (fun l ->
                   let m = Regex.Match(l, "fold along (.)=(\\d+)")
                   match m.Groups[1].Value with
                   | "x" -> (int m.Groups[2].Value, 0)
                   | "y" -> (0, int m.Groups[2].Value)
                )                   
  points, folds

let test = readInput "test13.txt"
let input = readInput "input13.txt"

let doFold points (xf, yf) =
  points
  |> Set.map (fun (x, y) ->
                 match xf, yf with
                 | 0, yf when y < yf -> (x, y)
                 | 0, yf -> (x, 2 * yf - y)
                 | xf, 0 when x < xf -> (x, y)
                 | xf, 0 -> (2 * xf - x, y))

let part1 (points, folds) =
  doFold points (List.head folds)
  |> Set.count

part1 input

let part2 (points, folds) =
  let points = List.fold doFold points folds
  let cols = points |> Seq.map fst |> Seq.max 
  let rows = points |> Seq.map snd |> Seq.max

  printfn ""
  for y = 0 to rows do
    for x = 0 to cols do
      if Set.contains (x, y) points then
        printf "#"
      else
        printf "."
    printfn ""

part2 input
