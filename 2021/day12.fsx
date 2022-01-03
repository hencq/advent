open System.IO

type Graph = Map<string, string list>

let readInput fname =
  let addEdge a b graph =
    match Map.tryFind a graph with
    | Some bs -> Map.add a (b :: bs) graph
    | None -> Map.add a [b] graph
  File.ReadAllLines fname
  |> Seq.map (fun l -> l.Split("-"))
  |> Seq.fold
       (fun m [|a; b|] ->
         m
         |> addEdge a b
         |> addEdge b a)
       Map.empty

let test = readInput "test12.txt"
let test2 = readInput "test12-2.txt"
let input = readInput "input12.txt"

let rec walk visited count node (graph : Graph) =
  if node = "end" then
    count + 1, graph
  else
    let visited = if node = node.ToLower()
                    then
                      Set.add node visited
                    else
                      visited

    ((count, graph), graph.[node])
    ||> List.fold
          (fun (c, g) n ->
             if Set.contains n visited then
               c, g
             else
               walk visited c n g)
    
let part1 input = walk Set.empty 0 "start" input |> fst
part1 input

let rec walk2 visited doubled (node : string) (graph : Graph) count =
  let isSmall (node : string) = node = node.ToLower()
    
  if node = "end" then
    count + 1
  else
    (count, graph.[node])
    ||> List.fold
          (fun c n ->
             match (isSmall n), (Set.contains n visited), doubled with
             | false, _, _ -> walk2 visited doubled n graph c
             | true, _, _ when n = "start" -> c
             | true, true, false -> walk2 visited true n graph c
             | true, true, true -> c
             | true, false, _ -> walk2 (Set.add n visited) doubled n graph c
           )

let part2 input = walk2 Set.empty false "start" input 0

part2 input
