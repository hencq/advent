open System.IO

type Point = int * int
type Line = { From: Point; To: Point }


let readInput fname =
  fname
  |> File.ReadAllLines
  |> Array.map
       (fun l ->
         l.Replace(" ", "").Split("->")
         |> Array.map
              (fun pt ->
                match pt.Split(",") |> Array.map int with
                | [| x; y |] -> (x, y))
         |> fun [| p1; p2 |] -> { From = p1; To = p2 })
  |> List.ofArray


let test = readInput "test5.txt"

let removeDiagonals lines =
  lines
  |> List.filter
       (fun line ->
         match line with
         | { From = (x0, y0); To = (x1, y1) } when x0 = x1 -> true
         | { From = (x0, y0); To = (x1, y1) } when y0 = y1 -> true
         | _ -> false)


let linePoints { From = (x0, y0); To = (x1, y1) } =
  let xs =
    (if x0 < x1 then
       seq { for x in x0 .. x1 -> x }
     elif x0 = x1 then
       Seq.initInfinite (fun _ -> x0)
     else
       seq { for x in x0 .. -1 .. x1 -> x })

  let ys =
    (if y0 < y1 then
       seq { for y in y0 .. y1 -> y }
     elif y0 = y1 then
       Seq.initInfinite (fun _ -> y0)
     else
       seq { for y in y0 .. -1 .. y1 -> y })

  Seq.zip xs ys |> List.ofSeq

test
|> removeDiagonals
|> List.collect linePoints
|> countPoints

let countPoints points =
  (Map.empty, points)
  ||> List.fold
        (fun counter pt ->
          counter
          |> Map.change
               pt
               (fun c ->
                 match c with
                 | Some c -> Some(c + 1)
                 | None -> Some 1))

let countCrossings lines =
  lines
  |> List.collect linePoints
  |> countPoints
  |> Map.filter (fun _ c -> c >= 2)
  |> Map.count

let part1 lines =
  lines |> removeDiagonals |> countCrossings

part1 test
let input = readInput "input5.txt"
part1 input

let part2 = countCrossings
part2 input
