open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pat str =
    let m = Regex.Match(str, pat)
    if m.Success then
        [ for g in m.Groups.Values do g.Value ] |> List.tail |> Array.ofList |> Some
    else None

type Point = int * int

let parseLine l =
    match l with
    | Regex "Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)" groups ->
        ((int groups[0], int groups[1]), (int groups[2], int groups[3]))
    | _ -> failwith "Can't parse line"

let readInput fname =
    fname
    |> File.ReadAllLines
    |> Array.map parseLine

let mark row (sensor, beacon) =
    let (sx, sy) = sensor
    let (bx, by) = beacon
    let n = max 0 ((abs (by - sy)) + (abs (bx - sx)) - (abs (row - sy)))
    if n > 0 then Some (sx - n, sx + n) else None
    
let findMarked row sensors =
    Array.choose (mark row) sensors
    |> Array.sortBy fst
 
let findGap marked =
    let marked = Array.toList marked
    let rec loop (a, b) marked =
        match marked with
        | [] -> None
        | (c, d) :: tl -> if c > b + 1 then Some (b + 1) else loop (a, (max b d)) tl
    loop (List.head marked) (List.tail marked)
                             
let findBeacon rows sensors =
    let rec loop y =
        if y = rows then None
        else
            match sensors |> findMarked y |> findGap with
            | Some x -> Some (x, y)
            | None -> loop (y + 1)
    loop 0

readInput "input15.txt"
|> findMarked 2000000
|> Array.reduce (fun (a, b) (c, d) -> (a, max b d))
|> fun (a, b) -> b - a

readInput "input15.txt"
|> findBeacon 4000000
|> function |Some (x, y) -> 4000000UL * (uint64 x) + (uint64 y) |None -> 0UL
