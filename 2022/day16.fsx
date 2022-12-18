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
let solve cave (targets : int[]) startTime =
    let dists = dists cave
    let cost i j = dists[targets[i]][targets[j]] + 1
    let valve i = cave.graph[targets[i]]
    let len = Array.length targets
    let memo = Array.init len (fun _ -> Array.create (1 <<< len) None)
    let q = new Queue<_>()
    let rec bfs best =
        if q.Count = 0 then best
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
                match memo[i][visited] with
                | Some bound when bound > released -> bfs best 
                | _ ->
                    memo[i][visited] <- Some released
                    for j = 0 to len - 1 do
                        if visited &&& (1 <<< j) = 0 then
                            q.Enqueue(visited, j, (time - (cost i j)), released)
                    if released > best then bfs released else bfs best
    for i = 0 to len - 1 do
        let t = startTime - dists[cave.indices["AA"]][targets[i]] - 1
        q.Enqueue((0, i, t, 0))
    bfs 0 
                                      
let part1 cave =
    let targets = cave.graph |> Array.mapi (fun i v -> if v.flow > 0 then Some i else None) |> Array.choose id
    solve cave targets 30

let bitLength n =
    let rec loop n i =
        if n = 0 then i
        elif n &&& 1 = 1 then loop (n >>> 1) (i + 1)
        else loop (n >>> 1) i
    loop n 0

let part2 cave =
    let targets = cave.graph |> Array.mapi (fun i v -> if v.flow > 0 then Some i else None) |> Array.choose id
    seq {
        for toVisit = 0 to (1 <<< (Array.length targets)) / 2 - 1 do
            // Not sure if this optimization is always valid, but it works on the input
            // Only consider pairs of sets of equal length, i.e. where you and elephant do the same amount of work
            if bitLength toVisit = (Array.length targets) / 2 then
                let targets1 = targets |> Array.mapi (fun i v -> if toVisit &&& (1 <<< i) = 0 then Some v else None) |> Array.choose id
                let targets2 = targets |> Array.mapi (fun i v -> if toVisit &&& (1 <<< i) > 0 then Some v else None) |> Array.choose id
                (solve cave targets1 26) + (solve cave targets2 26)
    }
    |> Seq.max

    

part1 test
part2 test

part1 input                         
part2 input

