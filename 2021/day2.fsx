open System.IO
open System.Text.RegularExpressions

let test =
  [ "forward 5"
    "down 5"
    "forward 8"
    "up 3"
    "down 8"
    "forward 2" ]

let input = File.ReadAllLines "input2.txt"

type Direction =
  | Forward of int
  | Down of int
  | Up of int

let (|MatchRegex|_|) patt input =
  let m = Regex.Match(input, (patt + " (\\d+)"))
  if m.Success then Some (int m.Groups[1].Value)
  else None

let readDirections dirs =
  dirs
  |> Seq.map
       (fun dir ->
         match dir with
         | MatchRegex "forward" dist -> Forward dist
         | MatchRegex "down" dist -> Down dist
         | MatchRegex "up" dist -> Up dist
         | _ -> Forward 0)

let addDirection (x, y) dir =
  match dir with
  | Forward dx -> (x + dx, y)
  | Down dy -> (x, y + dy)
  | Up dy -> (x, y - dy)

let ans1 input =
  input
  |> readDirections
  |> Seq.fold addDirection (0, 0)
  |> fun (x, y) -> x * y

ans1 input

let addAim (x, y, aim) dir =
  match dir with
  | Forward dx -> (x + dx, y + aim * dx, aim)
  | Down daim -> (x, y, aim + daim)
  | Up daim -> (x, y, aim - daim)

let ans2 input =
  input
  |> readDirections
  |> Seq.fold addAim (0, 0, 0)
  |> fun (x, y, _) -> x * y

ans2 input
