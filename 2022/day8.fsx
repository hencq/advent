open System.IO

let readInput fname =
    File.ReadAllLines fname
    |> Array.map (fun x -> x |> Seq.map (sprintf "%c" >> int) |> Seq.toArray)
    |> Array.mapi (fun y r -> r |> Array.mapi (fun x c -> ((x, y), c)))

let test = readInput "test8.txt"
let input = readInput "input8.txt"

let lines forest =
    let rows = forest
    let revrows = rows |> Array.map Array.rev
    let cols = Array.transpose rows
    let revcols = cols |> Array.map Array.rev
    Array.concat [ rows; revrows; cols; revcols ]

let neighbors line =
    let len = Array.length line
    let neighbors = Array.create len -1
    for i = 1 to len - 1 do
        neighbors[i] <- max neighbors[i - 1] (snd line[i - 1])
    line
    |> Array.mapi (fun i (xy, v) -> (xy, v, neighbors[i]))
    
let tallest line =
    line
    |> neighbors
    |> Array.choose (fun (xy, v, n) -> if n < v then Some xy else None)

let visible forest =
    lines forest
    |> Array.collect tallest 
    |> Set.ofArray
    |> Set.count
    

visible input

let ranges (line : ((int * int) * int)[]) =
    let rec range sum i (xy, v) =
        if i = 0 then (xy, sum)
        elif snd line[i-1] >= v then (xy, sum + 1)
        else range (sum + 1) (i - 1) (xy, v)
    line |> Array.mapi (range 0)

let scenic forest =
    lines forest
    |> Array.collect ranges
    |> Array.groupBy fst
    |> Array.map (fun (_, vs) -> vs |> Array.map snd |> Array.reduce (*))
    |> Array.max

scenic input
