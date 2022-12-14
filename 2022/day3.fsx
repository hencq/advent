open System.IO

let test = "vJrwpWtwJgWrhcsFMMfFFhFp"

let priority c =
    if c >= 'a' && c <= 'z' then int c - int 'a' + 1
    else int c - int 'A' + 27
    
let parseLine (l : string) =
    let len = l.Length
    let c1 = l[0..len/2-1]
    let c2 = l[len/2..]
    (c1 |> Seq.toArray |> Set.ofArray, c2 |> Seq.toArray |> Set.ofArray)
    ||> Set.intersect
    |> Set.map priority
    |> Seq.sum

let part1 =
    File.ReadAllLines "input3.txt"
    |> Array.map parseLine
    |> Array.sum

parseLine test

let part2 =
    File.ReadAllLines "input3.txt"
    |> Array.chunkBySize 3
    |> Array.map (fun xs -> xs |> Array.map (fun x -> x |> Seq.toArray |> Set.ofArray))
    |> Array.map (fun xs -> xs |> Array.reduce Set.intersect |> Set.map priority |> Seq.sum)
    |> Array.sum
