open System.IO
open System.Collections.Generic

type Point = int * int

let test =
    [| "498,4 -> 498,6 -> 496,6"
       "503,4 -> 502,4 -> 502,9 -> 494,9" |]

let parsePath (l : string) =
    let points = 
        [ for p in l.Split(" -> ") do
              let xy = p.Split(",")
              yield (int xy[0], int xy[1]) ] 
    (([], List.head points), List.tail points)
    ||> List.fold (fun (path, (x0, y0)) (x1, y1) ->
                        path
                        |> List.append
                              [ for x = (min x0 x1) to (max x0 x1) do
                                  for y = (min y0 y1) to (max y0 y1) do
                                      yield (x, y) ]
                        |> fun path -> (path, (x1, y1)))
    |> fst
    |> List.toArray

let parse lines =
    lines
    |> Array.collect parsePath

(* To save time we use a map of previous cells because each grain of sand will follow the same path
as the previous grain up to the previous cell. *)
let rec step (mp : HashSet<_>) (prevs : Dictionary<Point, Point>) isFree maxy ((x, y) as pt) =
    let step next =
        prevs.Add(next, pt)
        step mp prevs isFree maxy next
    if y > maxy then None
    elif not <| isFree pt then None
    elif isFree (x, y + 1) then step (x, y + 1)
    elif isFree (x - 1, y + 1) then step (x - 1, y + 1)
    elif isFree (x + 1, y + 1) then step (x + 1, y + 1)
    else
        mp.Add(pt) |> ignore
        Some (pt, prevs[pt])

let part1 (init : (int * int)[]) =
    let maxy = Seq.map snd init |> Seq.max
    let mp = new HashSet<_>(init)
    let isFree pt = mp.Contains pt |> not
    let prevs = new Dictionary<_,_>()
    let src = (500, 0)
    Seq.unfold (step mp prevs isFree maxy) src |> Seq.length

let part2 (init : (int * int)[]) =
    let maxy = Seq.map snd init |> Seq.max |> (+) 2
    let mp = new HashSet<_>(init)
    let isFree (x, y) = y < maxy && not (mp.Contains (x, y))
    let prevs = new Dictionary<_,_>()
    let src = (500, 0)
    prevs.Add(src, src)
    Seq.unfold (step mp prevs isFree maxy) src |> Seq.length
    
File.ReadAllLines "input14.txt"
|> parse
|> part1

File.ReadAllLines "input14.txt"
|> parse
|> part2

#time
