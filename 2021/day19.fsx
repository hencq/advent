open System.IO

let readInput fname =
  File.ReadAllText fname
  |> fun s -> s.Trim().Split("\n\n")
  |> Array.map
       (fun block ->
          block.Split("\n")
          |> Array.tail
          |> Array.map (fun l -> l.Split(",") |> Array.map int))

let test = readInput "test19.txt"
let input = readInput "input19.txt"

let distances (cube : int[][]) =
  let dist a1 a2 =
    Array.map2 (-) a1 a2
    |> Array.map (fun x -> pown x 2)
    |> Array.sum
  cube
  |> Array.map
      (fun a ->
         cube
         |> Array.map (fun b -> dist a b)
         |> set)
       
let mmul (m1 : int[][]) (m2 : int[][]) =
  let n = Array.length m1
  let m = Array.length m1[0]
  let p = Array.length m2[0]
  let res = Array2D.create n p 0
  for i = 0 to n - 1 do
    for j = 0 to p - 1 do
      let mutable sum = 0
      for k = 0 to m - 1 do
        sum <- sum + m1.[i].[k] * m2.[k].[j]
      res.[i, j] <- sum
  [| for i in 0 .. n - 1 -> res[i, *] |]

// Generate all 24 rotation matrices
let rots =
  let I = [|[|1;0;0|];[|0;1;0|];[|0;0;1|]|]
  let rotX = [| [| 1; 0; 0|]; [|0; 0; 1|]; [|0; -1; 0|] |]
  let rotY = [| [| 0; 0; -1|]; [|0; 1; 0|]; [|1; 0; 0|] |]
  let rotZ = [| [| 0; 1; 0|]; [|-1; 0; 0|]; [|0; 0; 1|] |]
  let x2 m = m |> mmul m 
  let x3 m = m |> mmul m |> mmul m 
  let turn axis m =
    let m' = mmul m axis
    let m'' = mmul m' axis
    let m''' = mmul m'' axis
    [ m; m'; m''; m''' ]
    
  let rots =
    [ [ I; x2 rotY ] |> List.collect (turn rotX)
      [ rotY; x3 rotY ] |> List.collect (turn rotZ)
      [ rotZ; x3 rotZ ] |> List.collect (turn rotY) ]
  rots |> List.concat

let vdiff v1 v2 =
  (v1, v2)
  ||> Array.map2 (fun p1 p2 -> p1 - p2)
  
let vadd v1 v2 =
  (v1, v2) ||> Array.map2 (+)

let tryMatchPt (dist1 : Set<int>) (dists2 : Set<int>[]) =
  let len = dists2.Length
  let rec loop i =
    if i = len then
      None
    else
      let overlap = Set.intersect dist1 dists2.[i] |> Set.count
      if overlap >= 12 then
        Some i
      else
        loop (i + 1)
  loop 0

let tryFindDiff (cube1 : int[][]) (dists1 : Set<int>[]) (cube2 : int[][]) (dists2 : Set<int>[]) =
  let len = cube1.Length
  let rec loop i diffOpt =
    if i = len then
      diffOpt
    else
      match tryMatchPt dists1.[i] dists2  with
      | None -> loop (i + 1) diffOpt
      | Some j ->
          let d' = vdiff cube1.[i] cube2.[j]
          match diffOpt with
          | None -> loop (i + 1) (Some d')
          | Some d when d = d' -> loop (i + 1) diffOpt
          | _ -> None
  loop 0 None 

type Scanner =
  { Pos : int[]
    Beacons : int[][] }

let findTranslation cube1 dists1 cube2 dists2 =
  let tryRot rot =
    let cube2' = mmul cube2 rot
    match tryFindDiff cube1 dists1 cube2' dists2 with
    | None -> None
    | Some diff -> Some { Pos = diff; Beacons = (Array.map (vadd diff) cube2') }
  rots
  |> Seq.tryPick tryRot
  

let canonicalize cubes =
  let distMap = Array.map distances cubes
  let len = Array.length cubes
  let indices = [ 0 .. len - 1 ]
  let canon = Array.create len None
  
  canon.[0] <- Some { Pos = [| 0; 0; 0 |]; Beacons = cubes.[0] }

  let rec walk n =
    indices
    |> List.iter (fun i ->
                    match canon[n], canon[i] with
                    | Some scanner, None ->
                        match findTranslation
                                scanner.Beacons distMap[n]
                                cubes[i] distMap[i] with
                        | None -> ()
                        | Some scanner ->
                            canon[i] <- Some scanner
                            walk i
                    | _ -> ())
  walk 0
  canon
  
let scanners input = canonicalize input |> Array.choose id
let beacons scanners =
  scanners
  |> Array.collect (fun s -> s.Beacons)
  |> Array.distinct
  
let part1 input =
  input
  |> scanners
  |> beacons
  |> Array.length


let maxDist scanners =
  let dist s1 s2 = Array.map2 (-) s1.Pos s2.Pos |> Array.map abs |> Array.sum 
  scanners
  |> Array.collect (fun s1 -> Array.map (dist s1) scanners)
  |> Array.max

let canon = scanners input
canon |> beacons |> Array.length
canon |> maxDist
