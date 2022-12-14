open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pat str =
    let m = Regex.Match(str, pat)
    if m.Success then
        [ for m in m.Groups -> m.Value ] |> List.tail |> List.toArray |> Some
    else None
    
type Token = |Open |Close |Num of int
type Packet = |Int of int |Series of Packet list

let tokenize str =
    let rec loop tokens str =
        match str with
        | "" -> List.rev tokens
        | Regex "^\[" _ -> loop (Open :: tokens) str[1..]
        | Regex "^]" _ -> loop (Close :: tokens) str[1..]
        | Regex "^(\d+)" groups -> loop ((Num (int groups[0])) :: tokens) str[groups[0].Length..]
        | Regex "^," _ -> loop tokens str[1..]
        | _ -> loop tokens str[1..]
    loop [] str

let rec parse tokens =
    match tokens with
    | [] -> (Series [])
    | hd :: tl ->
        match hd with
        | Num n -> (Int n, tl)
        | Open -> parseSeries tl []
        | Close -> failwith "Unexpected ]"

and parseSeries tokens contents =
    match tokens with
    | hd :: tl ->
        match hd with
        | Num n -> parseSeries tl ((Int n) :: contents)
        | Close -> (Series (List.rev contents), tl)
        | Open -> let (x, tl') = parseSeries tl []
                  parseSeries tl' (x :: contents)
    | [] -> failwith "Open without a close"

type Comp = |Lt |Gt |Eq

let rec compare p1 p2 =
    match p1, p2 with
    | Int a, Int b -> compareInts a b
    | Series xs, Series ys -> compareSeries xs ys
    | Int _ as n, Series ys -> compareSeries [n] ys
    | Series xs, (Int _ as n) -> compareSeries xs [n]

and compareInts a b =
    if a < b then Lt
    elif a > b then Gt
    else Eq

and compareSeries xs ys =
    match xs, ys with
    | [], y :: ys' -> Lt
    | x :: xs', [] -> Gt
    | [], [] -> Eq
    | x :: xs', y :: ys' ->
        match compare x y with
        | Lt -> Lt
        | Gt -> Gt
        | Eq -> compareSeries xs' ys'

let compareStrings s1 s2 =
    let p1 = s1 |> tokenize |> parse |> fst
    let p2 = s2 |> tokenize |> parse |> fst
    compare p1 p2

let readInput fname =
    fname
    |> File.ReadAllText
    |> fun s -> s.Split("\n\n")
    |> Array.map (fun ps ->
                      ps.Split("\n")
                      |> fun x -> (x[0], x[1]))

let part1 fname =
    fname
    |> readInput
    |> Array.map (fun (a,b) -> compareStrings a b)
    |> Array.mapi (fun i cmp -> (i, cmp))
    |> Array.choose (function
                     | i, Lt -> Some (i + 1)
                     | _, _ -> None)
    |> Array.sum


let part2 fname =
    let div1 = "[[2]]"
    let div2 = "[[6]]"
    let sorted =
        fname
        |> File.ReadAllLines
        |> Array.filter (fun x -> x <> "")
        |> Array.append [| div1; div2 |]
        |> Array.sortWith (fun a b ->
                               match compareStrings a b with
                               | Eq -> 0
                               | Lt -> -1
                               | Gt -> 1)
    let i1 = Array.findIndex ((=) div1) sorted
    let i2 = Array.findIndex ((=) div2) sorted
    (i1 + 1) * (i2 + 1)
    
part1 "input13.txt"
part2 "input13.txt"
