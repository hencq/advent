open System.IO

let readInput fname =
  File.ReadAllLines fname
  |> Array.map
       (fun (l: string) ->
         l.Trim().Split(" | ")
         |> Array.map (fun p -> p.Split(" "))
         |> fun parts -> (parts.[0], parts.[1]))

let test = readInput "test8.txt"
let input = readInput "input8.txt"

let part1 input =
  input
  |> Seq.map snd
  |> Seq.map
       (fun out ->
         out
         |> Seq.map (fun digit -> String.length digit)
         |> Seq.filter (fun len -> Set.contains len <| set [ 2; 3; 4; 7 ])
         |> Seq.length)
  |> Seq.sum

let digits =
  [ "abcefg"
    "cf"
    "acdeg"
    "acdfg"
    "bcdf"
    "abdfg"
    "abdefg"
    "acf"
    "abcdefg"
    "abcdfg" ]

let digitMap =
  Seq.zip digits (seq { 0 .. 9 }) |> Map.ofSeq

let lenMap =
  digits
  |> Seq.map Set.ofSeq
  |> Seq.groupBy Set.count
  |> Map.ofSeq
  |> Map.map (fun _ sets -> Set.intersectMany sets)

let rec propagate mapping =
  mapping
  |> Map.filter (fun _ st -> Set.count st = 1)
  |> Map.map (fun _ st -> st |> Set.toList |> List.head)
  |> Map.fold
       (fun state k fix ->
         state
         |> Map.map (fun k2 v -> if k = k2 then v else Set.remove fix v))
       mapping
  |> fun m -> if m = mapping then m else propagate m

let createMapping entry =
  (fst entry)
  |> Seq.map Set.ofSeq
  |> Seq.groupBy Set.count
  |> Seq.collect
       (fun (n, sets) ->
         sets
         |> Set.intersectMany
         |> Set.map (fun s -> (s, lenMap.[n])))
  |> Seq.groupBy fst
  |> Seq.map
       (fun (ch, sets) ->
         sets
         |> Seq.map snd
         |> Set.intersectMany
         |> fun s -> (ch, s))
  |> Map.ofSeq
  |> propagate
  |> Map.map (fun _ v -> v |> Set.toList |> List.head)

let unscramble mapping word =
  word
  |> Seq.map (fun ch -> Map.find ch mapping)
  |> Seq.sort
  |> Seq.toArray
  |> System.String
  |> fun w -> digitMap.[w]

let convertEntry entry =
  let mapping = createMapping entry

  (snd entry)
  |> Seq.map (unscramble mapping)
  |> Seq.map2 (*) [ 1000; 100; 10; 1 ]
  |> Seq.sum

let part2 entries =
  entries |> Array.map convertEntry |> Array.sum

part1 input
part2 input
