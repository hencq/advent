open System.IO
open System.Collections.Generic

let readMap fname =
    let mutable S = (0, 0)
    let mutable E = (0, 0)
    let elevation = 
        fname
        |> File.ReadAllLines
        |> Array.mapi (fun y row ->
                           row.ToCharArray()
                           |> Array.mapi (fun x ch ->
                                              if ch = 'S' then
                                                  S <- (x, y)
                                                  0
                                              elif ch = 'E' then
                                                  E <- (x, y)
                                                  25
                                              else
                                                  (int ch) - (int 'a')))
    (elevation, S, E)

let test = readMap "test12.txt"

let find (elev, S, E) =
   let todo = new Queue<_>()
   let maxy = (Array.length elev) - 1
   let maxx = (Array.length elev[0]) - 1
   let adj (x, y) =
       [(0, 1); (0, -1); (1, 0); (-1, 0)]
       |> List.map (fun (dx, dy) -> (x + dx, y + dy))
       |> List.filter (fun (x', y') -> x' >= 0 && y' >= 0 && x' <= maxx && y' <= maxy)
       |> List.filter (fun (x', y') -> elev[y'][x'] <= elev[y][x] + 1)
   todo.Enqueue((S, 0))
   let rec loop path seen =
       if todo.Count = 0 then path
       else
           let (n, sofar) = todo.Dequeue()
           if n = E then sofar
           else
               let adjs = adj n
               for a in adjs do
                   if not (Set.contains a seen) then todo.Enqueue((a, sofar + 1))
               let seen' = List.fold (fun s a -> Set.add a s) seen adjs
               loop sofar seen'
   loop 0 (Set.empty)


let find2 (elev, S, E) =
   let todo = new Queue<_>()
   let maxy = (Array.length elev) - 1
   let maxx = (Array.length elev[0]) - 1
   let adj (x, y) =
       [(0, 1); (0, -1); (1, 0); (-1, 0)]
       |> List.map (fun (dx, dy) -> (x + dx, y + dy))
       |> List.filter (fun (x', y') -> x' >= 0 && y' >= 0 && x' <= maxx && y' <= maxy)
       |> List.filter (fun (x', y') -> elev[y'][x'] >= elev[y][x] - 1)
   todo.Enqueue((E, 0))
   let rec loop seen =
       if todo.Count = 0 then (failwith "No path found")
       else
           let ((x, y), sofar) = todo.Dequeue()
           if elev[y][x] = 0 then ((x,y), sofar)
           else
               let adjs = adj (x, y) 
               for a in adjs do
                   if not (Set.contains a seen) then todo.Enqueue((a, sofar + 1))
               let seen' = List.fold (fun s a -> Set.add a s) seen adjs
               loop seen'
   loop (Set.empty) |> snd

readMap "input12.txt"
|> find

readMap "input12.txt"
|> find2
