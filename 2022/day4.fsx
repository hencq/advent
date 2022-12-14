open System.IO

let parseRange (r : string) =
    let ps = r.Split("-")
    (int ps[0], int ps[1])
    
let parseLine (l : string) =
    let es = l.Split(",")
    (parseRange es[0], parseRange es[1])
    
parseLine "4-90,1-4"

let isNested pair =
    match pair with
        | (a, b), (c, d) when a <= c && b >= d -> true
        | (a, b), (c, d) when c <= a && d >= b -> true
        | _ -> false

let part1 =
    File.ReadAllLines "input4.txt"
    |> Array.map parseLine
    |> Array.filter isNested
    |> Array.length

let overlaps = function
    | (a, b), (c, d) when a >= c && a <= d -> true
    | (a, b), (c, d) when b >= c && b <= d -> true
    | (a, b), (c, d) when c >= a && c <= b -> true
    | (a, b), (c, d) when d >= a && d <= b -> true
    | _ -> false

let part2 =
    File.ReadAllLines "input4.txt"
    |> Array.map parseLine
    |> Array.filter overlaps
    |> Array.length
