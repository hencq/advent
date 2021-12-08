open System.IO

let readInput fname =
  File.ReadAllLines fname
  |> Array.map (fun (l : string) ->
                l.Trim().Split(" | ")
                |> Array.map (fun p -> p.Split(" "))
                |> fun parts -> (parts.[0], parts.[1]))

let test = readInput "test8.txt"
let input = readInput "input8.txt"

let part1 input =
  input
  |> Seq.map snd
  |> Seq.map (fun out ->
              out
              |> Seq.map (fun digit -> String.length digit)
              |> Seq.filter (fun len -> Set.contains len <| set [2; 3; 4; 7])
              |> Seq.length)
  |> Seq.sum

part1 input

let digits =
  ["abcefg"; "cf"; "acdeg"; "acdfg"; "bcdf"; "abdfg"; "abdefg"; "acf";
   "abcdefg"; "abcdfg"]

let digitMap =
  Seq.zip digits (seq {0 .. 9})
  |> Map.ofSeq
  
let lenMap =
   digits
   |> Seq.map (fun word -> (String.length word, Set.ofSeq word))
   |> Seq.fold
        (fun mapping (len, options) ->
         mapping
         |> Map.change len (fun optSet ->
                            match optSet with
                            | Some st -> Set.intersect st options |> Some
                            | None -> Some options))
        Map.empty
                         
let rec propagate mapping =
  let getFixed m =
    m
    |> Map.toSeq
    |> Seq.choose (fun (ch, vs) ->
                   if (Set.count vs) = 1 then
                     vs |> Set.toList |> List.head |> fun v -> Some (ch, v)
                   else
                     None)
  let fix = getFixed mapping
  let next =
    mapping
    |> Map.map (fun ch st ->
      (st, fix)
      ||> Seq.fold (fun st (fixch, fixval) ->
           if ch = fixch then
             st
           else
             Set.remove fixval st))

  if (next |> getFixed |> Seq.length) > (fix |> Seq.length) then
    propagate next
  else
    next
    |> Map.map (fun ch st -> st |> Set.toList |> List.head)
    |> Map.toSeq
    |> Seq.map (fun (a, b) -> (b, a))
    |> Map.ofSeq
    
let createMapping entry =
  (fst entry)
  |> Seq.collect (fun (word : string) ->
                    let options = word |> Set.ofSeq
                    lenMap.[word.Length]
                    |> Set.map (fun ch -> (ch, options)))
  |> Seq.fold
       (fun mapping (ch, options) ->
          mapping
          |> Map.change ch (fun optSet ->
                              match optSet with
                              | Some st -> Set.intersect st options |> Some
                              | None -> Some options))
       Map.empty
   |> propagate

    
let unscramble word mapping =
  word
  |> Seq.map (fun ch -> Map.find ch mapping)
  |> Seq.sort
  |> Seq.toArray
  |> System.String
  |> fun w -> digitMap.[w]

let convertEntry entry =
  let mapping = createMapping entry
  (snd entry)
  |> Seq.map (fun w -> unscramble w mapping)
  |> Seq.map2 (*) [1000; 100; 10; 1]
  |> Seq.sum

let part2 entries =
  entries
  |> Array.map convertEntry
  |> Array.sum
  
part2 input
