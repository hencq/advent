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

let dist a1 a2 =
  Array.map2 (-) a1 a2
  |> Array.map (fun x -> pown x 2)
  |> Array.sum

let distances (cube : int[][]) =
  cube
  |> Array.collect
      (fun a ->
         cube
         |> Array.map (fun b -> dist a b))
  |> set
       
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

let rotate v rot =
  let m = [| v |]
  (mmul m rot).[0]
  
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

let tryFindDiff (cube1 : int[][]) (dists1 : Set<int>) (cube2 : int[][]) (dists2 : Set<int>) rot =
  if (Set.intersect dists1 dists2 |> Set.count) > 66 then
    
  let len = cube1.Length
  let rec loop i diffOpt =
    if i = len then
      diffOpt
    else
      match tryMatchPt dists1.[i] dists2  with
      | None -> loop (i + 1) diffOpt
      | Some j ->
          let v2 = rotate cube2.[j] rot
          let d' = vdiff cube1.[i] v2
          match diffOpt with
          | None -> loop (i + 1) (Some d')
          | Some d when d = d' -> loop (i + 1) diffOpt
          | _ -> None
  loop 0 None 

type Scanner =
  { Pos : int[]
    Beacons : int[][] }

let findTranslation cube1 dists1 cube2 dists2 =
  let tryRot (a1, b1) (a2, b2) rot =
    let b1' = rotate b1 rot
    let b2' = rotate b2 rot
    let d1 = vdiff a1 b1'
    let d2 = vdiff a2 b2'
    if d1 = d2 then
      Some { Pos = d1; Beacons = (Array.map (vadd d1) (mmul cube2 rot)) }
    else
      None
  if (Set.intersect dists1 dists2 |> Set.count) > 66 then
    let pts1 =
      cube1
      |> Array.map (fun p ->
                          let d = Array.map (dist p) cube1 |> set
                          (p, d))
    let pts2 =
      cube2
      |> Array.map (fun p ->
                          let d = Array.map (dist p) cube2 |> set
                          (p, d))
    let pair1 :: pair2 :: [] =
      seq { for (p1, d1) in pts1 do
              for (p2, d2) in pts2 do
                if (Set.intersect d1 d2 |> Set.count) > 11 then
                  yield (p1, p2) }
      |> Seq.take 2
      |> List.ofSeq

    rots
    |> Seq.tryPick (tryRot pair1 pair2) 
  else
    None
  
findTranslation test[0] distMap[0] test[1] distMap[1]

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

#time
let canon = scanners input
canon |> beacons |> Array.length
canon |> maxDist
