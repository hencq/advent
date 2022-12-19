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

// Calculate all the distances between all the valves (including non-working valves)
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

// Solve traveling salesman by a bfs with pruning with bounds
let solve cave startTime =
    let targets = cave.graph |> Array.mapi (fun i v -> if v.flow > 0 then Some i else None) |> Array.choose id
    let dists = dists cave
    let cost i j = dists[targets[i]][targets[j]] + 1
    let valve i = cave.graph[targets[i]]
    let len = Array.length targets
    let memo = Array.init (1 <<< len) (fun _ -> Array.create len 0)
    let q = new Queue<_>()
    let rec bfs best =
        if q.Count = 0 then (best, memo)
        else
            let (visited, i, time, released) = q.Dequeue()
            if time <= 0 || visited = (1 <<< len) - 1 then
                bfs best
            else
                let released = released + time * (valve i).flow
                let visited = visited ||| (1 <<< i)
                // If we have visited set of valves S and are at valve i, we can't do
                // better than another path with a higher score at the same stage (i.e. with same S and same i)
                // so no need to continue the search
                if memo[visited][i] > released then bfs best 
                else
                    memo[visited][i] <- released
                    for j = 0 to len - 1 do
                        if visited &&& (1 <<< j) = 0 then
                            q.Enqueue(visited, j, (time - (cost i j)), released)
                    if released > best then bfs released else bfs best
    for i = 0 to len - 1 do
        let t = startTime - dists[cave.indices["AA"]][targets[i]] - 1
        q.Enqueue((0, i, t, 0))
    bfs 0 
                                      
let part1 cave =
    fst (solve cave 30)

let part2 cave =
    let (_, memo) = solve cave 26
    let len = memo[0] |> Array.length
    seq {
        for visit1 = 0 to (1 <<< len) / 2 do
            let p1 = memo[visit1] |> Array.max
            if p1 > 0 then
                for visit2 = visit1 + 1 to (1 <<< len) - visit1 - 1 do
                    if visit1 &&& visit2 = 0 then
                        let p2 = memo[visit2] |> Array.max
                        (p1 + p2)
    }
    |> Seq.max

    
#time
part1 test
part2 test

part1 input                         
part2 input

