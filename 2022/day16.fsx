open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

let (|Regex|_|) pat str =
    let m = Regex.Match(str, pat)
    if m.Success then
        [ for g in m.Groups.Values do g.Value ] |> List.tail |> Array.ofList |> Some
    else None

type Valve = { name : string; flow : int; tunnels : int[] }
type Graph = Valve[]
type Cave = { graph : Graph; indices : Map<string, int> }

let parseLine l =
    match l with
    | Regex "Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)" groups ->
        (groups[0], int groups[1], groups[2].Split(", "))
    | _ -> failwith (sprintf "Couldn't parse line %A" l)

let parse fname =
    let valves =
        fname
        |> File.ReadAllLines
        |> Array.map parseLine
    let indices = valves |> Array.mapi (fun i (n, _, _) -> (n, i)) |> Map.ofArray
    let graph = 
        valves
        |> Array.map (fun (n, f, ts) -> { name = n; flow = f; tunnels = ts |> Array.map (fun t -> indices[t]) })
    { graph = graph; indices = indices }

let test = "test16.txt" |> parse
let input = "input16.txt" |> parse

let dists (cave : Cave) =
    let len = Array.length cave.graph
    let dists = Array.init len (fun _ -> Array.create len 0)
    for i = 0 to len - 1 do
        for j in cave.graph[i].tunnels do
            dists[i][j] <- 1
            dists[j][i] <- 1

    for k = 0 to len - 1 do
        for i = 0 to len - 1 do
            for j = 0 to len - 1 do
                if i <> j && dists[i][k] > 0 && dists[k][j] > 0 then
                    if dists[i][j] = 0 then dists[i][j] <- dists[i][k] + dists[k][j]
                    else dists[i][j] <- min (dists[i][j]) (dists[i][k] + dists[k][j])

    dists

let tuple2 a b = (a, b)

let salesman cave =
    let dists = dists cave
    let valves = cave.graph |> Array.mapi tuple2 |> Array.choose (fun (i, v) -> if v.flow > 0 then Some i else None)
    let len = Array.length valves
    let memo = Array.init len (fun _ -> Array.create (1 <<< len) None)
    // value of traveling from 0 to i via nodes in S
    let rec value S i =
        //trivial case, straight from 0 to i
        if S = (1 <<< i) then
            let time = 30 - dists[cave.indices["AA"]][valves[i]] - 1
            let flow = time * cave.graph[valves[i]].flow
            (time, flow, [(time, cave.graph[valves[i]].name)])
        else
            match memo[i][S] with
            | Some v -> v
            | None -> 
                let Si = S &&& (~~~ (1 <<< i)) // remove i from S and call it Si
                let result =
                    [
                        for j in 0..(len-1) do             // for all j in Si
                            if (Si &&& (1 <<< j)) > 0 && j <> i then yield j
                    ]
                    |> List.fold
                        (fun (t, f, p) j ->
                            let (time', flow', path') = value Si j
                            let time'' = time' - dists.[valves[j]].[valves[i]] - 1
                            let flow'' = flow' + (time'' * cave.graph[valves[i]].flow)
                            if time'' >= 0  && flow'' > f then
                                (time'', flow'', (30 - time'', cave.graph[valves[i]].name) :: path')
                            else (t, f, p))
                        (0, 0, [])
                memo[i][S] <- Some result
                result

    seq {
        for S in 1..((1 <<< len)-1) do
            for e in 0..(len-1) do
               value S e 
    }
    |> Seq.maxBy (fun (_, v, _) -> v)

    
salesman test
salesman input

let solveDfs cave =
    let dists = dists cave
    let targets = cave.graph |> Array.mapi (fun i v -> if v.flow > 0 then Some i else None) |> Array.choose id
    let valve i = cave.graph[targets[i]]
    let len = Array.length targets
    let memo = Array.init len (fun _ -> Array.create (1 <<< len) None)
    let rec dfs visited time released i path =
        if time <= 0 || visited = (1 <<< len) - 1 then (released, path)
        else
            let released = released + time * (valve i).flow
            let visited = visited ||| (1 <<< i)
            match memo[i][visited] with
            | Some (bound, res) when bound > released -> res
            | _ ->
                let path' = (30 - time, (valve i).name) :: path
                let result =
                    seq {
                        yield (released, path')
                        for j = 0 to len - 1 do
                            if visited &&& (1 <<< j) = 0 then
                                yield dfs visited (time - dists[targets[i]][targets[j]] - 1) released j path'
                    }
                    |> Seq.maxBy fst
                memo[i][visited] <- Some (released, result)
                result
    seq {
        for i in 0..(len - 1) do
           let t = 30 - dists[cave.indices["AA"]][targets[i]] - 1
           dfs 0 t 0 i []
    }
    |> Seq.maxBy fst


let solveBfs cave =
    let dists = dists cave
    let targets = cave.graph |> Array.mapi (fun i v -> if v.flow > 0 then Some i else None) |> Array.choose id
    let valve i = cave.graph[targets[i]]
    let len = Array.length targets
    let memo = Array.init len (fun _ -> Array.create (1 <<< len) None)
    let q = new Queue<_>()
    let rec bfs bestScore bestPath =
        if q.Count = 0 then (bestScore, List.rev bestPath)
        else
            let (visited, i, time, released, path) = q.Dequeue()
            if time <= 0 || visited = (1 <<< len) - 1 then 
                bfs bestScore bestPath
            else
                let released = released + time * (valve i).flow
                let visited = visited ||| (1 <<< i)
                match memo[i][visited] with
                | Some bound when bound > released -> bfs bestScore bestPath
                | _ ->
                    memo[i][visited] <- Some released
                    let path' = (30 - time, (valve i).name) :: path
                    for j = 0 to len - 1 do
                        if visited &&& (1 <<< j) = 0 then
                            q.Enqueue(visited, j, (time - dists[targets[i]][targets[j]] - 1), released, path')
                    if released > bestScore then bfs released path' else bfs bestScore bestPath

    for i in 0..(len - 1) do
        let t = 30 - dists[cave.indices["AA"]][targets[i]] - 1
        q.Enqueue((0, i, t, 0, []))
    bfs 0 []

    
   

#time
solveBfs test 
solveBfs input


