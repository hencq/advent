open System.IO

type Direction = |Up |Down |Left |Right

let tail (xh, yh) (xt, yt) =
    let dx = xh - xt
    let dy = yh - yt
    if abs dx <= 1 && abs dy <= 1 then (xt, yt)
    else (xt + sign dx, yt + sign dy)

let movehead dir snake =
   let hd =
       match List.head snake, dir with
       | (x, y), Up -> (x, y + 1)
       | (x, y), Down -> (x, y - 1)
       | (x, y), Left -> (x - 1, y)
       | (x, y), Right -> (x + 1, y)
   (hd, List.tail snake)
   ||> List.mapFold (fun hd knot ->
                         let knot' = tail hd knot
                         (knot', knot'))
   |> fun (tl, _) -> hd :: tl

let rec move trail (dir, n) =
    if n = 0 then trail
    else
        let rope = List.head trail
        move ((movehead dir rope) :: trail) (dir, (n - 1))

let solve start path =
    ([start], path)
    ||> Seq.fold move
    |> Seq.map List.last
    |> Set.ofSeq
    |> Set.count

let parseRow (row : string) =
    let parts = row.Split(" ")
    let dir =
        match parts[0] with
        | "D" -> Down
        | "U" -> Up
        | "L" -> Left
        | "R" -> Right
    let n = int parts[1]
    (dir, n)

        
let test =
    [ (Right, 4);
      (Up, 4);
      (Left, 3);
      (Down, 1);
      (Right, 4);
      (Down, 1);
      (Left, 5);
      (Right, 2) ]

let path =
    File.ReadAllLines "input9.txt"
    |> Array.map parseRow


solve [(0,0); (0, 0)] path

let part2 = Array.create 10 (0, 0) |> Array.toList
solve part2 path



