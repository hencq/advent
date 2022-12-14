open System.IO
open System.Collections.Generic

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
    |> Set.ofArray
                      
let step1 _ (mp : HashSet<_>) =
    let maxy = mp |> Seq.map snd |> Seq.max
    let rec loop x y =
       if y > maxy then ()
       elif not (mp.Contains((x, y + 1)))then loop x (y + 1)
       elif not (mp.Contains((x - 1, y + 1))) then loop (x - 1) (y + 1)
       elif not (mp.Contains((x + 1, y + 1))) then loop (x + 1) (y + 1)
       else mp.Add((x, y)) |> ignore
    loop 500 0

let step2 floor (mp : HashSet<_>) =
    let isFree x y =
        (y < floor) && (not (mp.Contains((x, y))))
    let rec loop x y =
       if isFree x (y + 1) then loop x (y + 1)
       elif isFree (x - 1) (y + 1) then loop (x - 1) (y + 1)
       elif isFree (x + 1) (y + 1) then loop (x + 1) (y + 1)
       else mp.Add((x, y)) |> ignore
    if isFree 500 0 then loop 500 0 else ()

let fall step init =
    let floor = Seq.map snd init |> Seq.max |> fun x -> x + 2
    let mp = new HashSet<_>(init |> Set.toSeq)
    let rec loop i =
        let cnt = mp.Count
        step floor mp
        if mp.Count = cnt then i
        else loop (i + 1)
    loop 0

File.ReadAllLines "input14.txt"
|> parse
|> fall step2
