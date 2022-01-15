open System.IO
open System.Text.RegularExpressions

type Cuboid =
  { X0 : int
    X1 : int
    Y0 : int
    Y1 : int
    Z0 : int
    Z1 : int
    Switch : bool }
    
let readInput fname =
  let parseLine l =
    let m = Regex.Match(l, "(\\w+) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)")
    if m.Success then
      let vals = m.Groups |> Seq.map (fun g -> g.Value) |> Array.ofSeq
      let switch = vals[1] = "on"
      let x0 = int vals[2]
      let x1 = int vals[3]
      let y0 = int vals[4]
      let y1 = int vals[5]
      let z0 = int vals[6]
      let z1 = int vals[7]
      { X0 = min x0 x1; X1 = max x0 x1
        Y0 = min y0 y1; Y1 = max y0 y1
        Z0 = min z0 z1; Z1 = max z0 z1
        Switch = switch }
    else
      failwith "Parse error"
  File.ReadAllLines fname
  |> Seq.map parseLine

let intersect c1 c2 =
  let x0 = max c1.X0 c2.X0
  let x1 = min c1.X1 c2.X1
  let y0 = max c1.Y0 c2.Y0
  let y1 = min c1.Y1 c2.Y1
  let z0 = max c1.Z0 c2.Z0
  let z1 = min c1.Z1 c2.Z1
  if x1 >= x0 && y1 >= y0 && z1 >= z0 then
    Some { X0 = x0; X1 = x1;
           Y0 = y0; Y1 = y1;
           Z0 = z0; Z1 = z1;
           Switch = not c2.Switch }
  else
    None

let addCuboid cs c =
  let negs = cs |> List.choose (intersect c)
  if c.Switch then
    List.append cs (c :: negs)
  else
    List.append cs negs
    
let volume c =
  let l1 = int64 (c.X1 - c.X0 + 1)
  let l2 = int64 (c.Y1 - c.Y0 + 1)
  let l3 = int64 (c.Z1 - c.Z0 + 1)
  let size = l1 * l2 * l3
  if c.Switch then size else -size

let boot cuboids =
  cuboids
  |> List.ofSeq
  |> List.fold addCuboid []
  |> List.map volume
  |> List.sum
  
let part1 input =
  input
  |> Seq.filter (fun c ->
                   c.X0 >= -50 && c.X1 <= 50 &&
                   c.Y0 >= -50 && c.Y1 <= 50 &&
                   c.Z0 >= -50 && c.Z1 <= 50)
  |> boot

let test = readInput "test22.txt"
let input = readInput "input22.txt"
  
part1 input
boot input

