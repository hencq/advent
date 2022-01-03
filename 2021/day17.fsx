open System.IO

let part1 lowest =
  let alow = abs(lowest)
  alow * (alow - 1) / 2


part1 -73

let testVy y0 y1 vy =
  let rec loop vy y n m =
    //printfn "min: %A max: %A vy: %A y: %A" m n vy y
    if y < y0 then
      // beneath target
      m, (n - 1)
    elif y > y1 then
      // above target
      loop (vy - 1) (y + vy) (n + 1) m
    elif m < 0 then
      // first time on target
      loop (vy - 1) (y + vy) (n + 1) n
    else
      // successive on target
      loop (vy - 1) (y + vy) (n + 1) m

  loop vy 0 0 -1

let testVx x0 x1 n0 n1 vx =
  let step vx =
    if vx > 0 then vx - 1 else 0
    
  let rec loop n vx x =
    //printfn "n: %A vx: %A x: %A" n vx x
    if n < n0 then
      loop (n + 1) (step vx) (x + vx)
    elif n > n1 then
      false
    elif x >= x0 && x <= x1 then
      true
    else loop (n + 1) (step vx) (x + vx)

  loop 0 vx 0

let testVelocity x0 x1 y0 y1 vx vy =
  let n0, n1 = testVy y0 y1 vy
  if n0 > 0 then
    testVx x0 x1 n0 n1 vx
  else
    false
  
let part2 x0 x1 y0 y1 =
  seq { for x = 0 to x1 do
          for y = y0 to -y0 do
            yield (x, y) }
  |> Seq.filter (fun (vx, vy) -> testVelocity x0 x1 y0 y1 vx vy)


part2 253 280 -73 -46 |> Seq.length

