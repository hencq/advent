open System.IO

let readInput fname =
  File.ReadAllLines fname
  |> Array.map (fun l -> Seq.map (string >> int) l)
  |> array2D

let test = readInput "test11.txt"
let input = readInput "input11.txt"


let neighbors x y =
  [for dx in -1 .. 1 do
     for dy in -1 .. 1 do
       if not (dx = 0 && dy = 0) then yield (x + dx, y + dy)]

let inc v = v + 1

let changeCell grid f x y =
  if x < 0 || y < 0 then
    ()
  elif x >= (Array2D.length2 grid) || y >= (Array2D.length1 grid) then
    ()
  else
    grid[y, x] <- f grid[y, x]

let step1 (grid : int[,]) =
  let changeCell = changeCell grid
  let incNonZero v =
    if v > 0 then v + 1 else v
  Array2D.iteri (fun y x _ -> changeCell inc x y) grid

  let rec loop flashes =
    let mutable count = 0
    Array2D.iteri
      (fun y x v ->
        if v > 9 then 
          Seq.iter (fun (nx, ny) -> changeCell incNonZero nx ny)
                   (neighbors x y)
          grid[y, x] <- 0
          count <- count + 1)
      grid
    if count > 0 then
      loop (flashes + count)
    else
      flashes
  loop 0


let step (grid : int[,]) n =
  let grid = Array2D.copy grid
  let mutable count = 0
  for i = 1 to n do
    count <- count + step1 grid
  count, grid

let stepUntilSync (grid : int[,]) =
  let grid = Array2D.copy grid
  let goal = (Array2D.length1 grid) * (Array2D.length2 grid)
  let rec loop n =
    let flashes = step1 grid
    if flashes = goal then
      n
    else
      loop (n + 1)
  loop 1


step input 100
stepUntilSync input
