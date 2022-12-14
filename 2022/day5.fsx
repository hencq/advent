open System.IO
open System.Text.RegularExpressions

let parseState (block : string) =
    let isBox (_, c) =
        (c >= 'A' && c <= 'Z') || (c >= '1' && c <= '9')
    block.Split("\n")
    |> Array.rev
    |> Array.map (fun l -> l |> Seq.mapi (fun i c -> (i, c)))
    |> Seq.collect (Seq.filter isBox)
    |> Seq.groupBy fst
    |> Seq.map (snd >> Seq.map snd >> Seq.toList)
    |> Seq.map (fun col ->
                match col with
                | hd :: tl -> (int hd - int '1' + 1, List.rev tl)
                | _ -> failwith "Error")
    |> Map.ofSeq

let parseMove (line : string) =
    let m = Regex.Match(line, "move (\d+) from (\d+) to (\d+)")
    if m.Success then
        (m.Groups[1].Value |> int, m.Groups[2].Value |> int, m.Groups[3].Value |> int)
     else failwith (sprintf "Parse error: %A" line)
    

let parseInput fname =
    let blocks =
        File.ReadAllText fname
        |> fun x -> x.Split("\n\n")
    let state = parseState blocks[0]
    let moves =
        blocks[1].Split("\n")
        |> Array.filter (fun x -> x <> "")
        |> Array.map parseMove
    (state, moves)
    

parseInput "input5.txt"
|> fun (st, mvs) -> Array.fold doMove st mvs
|> Map.values
|> Seq.map List.head
|> Seq.toArray
|> fun x -> new System.String(x)

let rec doMove state (n, src, dst) =
    if n = 0 then state
    else
        let state' =
            match Map.find src state with
                | top :: stk -> state
                                |> Map.add src stk
                                |> Map.change dst
                                              (function
                                               | Some stk' -> Some (top :: stk')
                                               | _ -> failwith "No dst")
                | _ -> failwith "Empty stack"
        doMove state' (n - 1, src, dst)

let doMove2 state (n, src, dst) =
    match Map.find src state with
    | stk -> state
             |> Map.add src (List.removeManyAt 0 n stk)
             |> Map.change dst (function
                                | Some stk' -> Some (List.append (List.take n stk) stk')
                                | _ -> failwith "No dst")
                                                          

parseInput "input5.txt"
|> fun (st, mvs) -> Array.fold doMove2 st mvs
|> Map.values
|> Seq.map List.head
|> Seq.toArray
|> fun x -> new System.String(x)
