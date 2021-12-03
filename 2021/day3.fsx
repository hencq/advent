open System.IO

let test = [
  "00100"
  "11110"
  "10110"
  "10111"
  "10101"
  "01111"
  "00111"
  "11100"
  "10000"
  "11001"
  "00010"
  "01010"
]

let input = File.ReadAllLines "input3.txt" |> Array.toList

let count key table =
  table |> Map.change key (fun x ->
                           match x with
                           | Some c -> Some (c + 1)
                           | None -> Some 1)

let countBits state (bits : string) =
  Array.map2 count (bits.ToCharArray()) state
  
let countAll (bytes : string list) =
  let state = Array.init (String.length bytes[0]) (fun _ -> Map.empty) 
  List.fold countBits state bytes 

let makeConverter sorter =
  fun bytes ->
    countAll bytes
    |> Array.map (fun m ->
                    Map.toList m
                    |> List.sortBy sorter 
                    |> fun list -> match list with (b, _)::_ -> b) 
    |> System.String 
    |> fun s -> System.Convert.ToInt32(s, 2)

let gamma = makeConverter (fun (_, c) -> -c)
let epsilon = makeConverter (fun (_, c) -> c)

let part1 bytes = (gamma bytes) * (epsilon bytes)

part1 input

let transpose (bytes : string list) =
  let init = Array.init (String.length bytes[0]) (fun _ -> [])
  let folder state (byte : string) =
    Array.map2 (fun col b -> b::col) state (byte.ToCharArray())
  List.fold folder init bytes

transpose test

let getColumn (bytes : string list) col =
  let folder state (byte : string) =
    (byte[col])::state
  List.fold folder [] bytes

let mostCommon col =
  let rec count col ones zeroes =
    match col with
    | [] -> if ones >= zeroes then '1' else '0'
    | '1'::tl -> count tl (ones + 1) zeroes
    | '0'::tl -> count tl ones (zeroes + 1)
  count col 0 0

let leastCommon col =
  if mostCommon col = '1' then '0' else '1'

let filterBytes criteria (bytes : string list) =
  let cols = String.length bytes[0]
  let rec filter colnum bytes =
    if colnum = cols then bytes 
    elif List.length bytes = 1 then bytes
    else
      let col = getColumn bytes colnum
      let target = criteria col
      bytes
      |> List.filter (fun byte ->
                            byte[colnum] = target)
      |> filter (colnum + 1)
  System.Convert.ToInt32((filter 0 bytes)[0], 2)

let oxygen = filterBytes mostCommon
let co2 = filterBytes leastCommon

let part2 bytes = (oxygen bytes) * (co2 bytes)

part2 input
