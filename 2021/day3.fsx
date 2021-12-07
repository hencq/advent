open System.IO

let test =
  [ "00100"
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
    "01010" ]

let input =
  File.ReadAllLines "input3.txt" |> Array.toList

// get a column of bits from the list of bytes by col number
let getColumn (bytes: string list) col =
  bytes
  |> List.fold (fun state byte -> byte.[col] :: state) [] 

// count the bits and return the most common
let mostCommon col =
  let rec count col ones zeroes =
    match col with
    | [] -> if ones >= zeroes then '1' else '0'
    | '1' :: tl -> count tl (ones + 1) zeroes
    | '0' :: tl -> count tl ones (zeroes + 1)
  count col 0 0

let leastCommon col =
  if mostCommon col = '1' then '0' else '1'

// get each column and determine whether it's 1 or 0
let collectBits criteria (bytes: string list) =
  let cols = String.length (bytes.[0])
  List.init
    cols
    (fun i ->
      let col = getColumn bytes i
      criteria col)
  |> Array.ofList
  |> System.String
  |> fun s -> System.Convert.ToInt32(s, 2)

let gamma = collectBits mostCommon
let epsilon = collectBits leastCommon

let part1 bytes = (gamma bytes) * (epsilon bytes)

part1 input

// filter the bytes based on the given criteria
let filterBytes criteria (bytes: string list) =
  let cols = String.length (bytes.[0])

  let rec filter colnum bytes =
    if colnum = cols then
      bytes
    elif List.length bytes = 1 then
      bytes
    else
      let col = getColumn bytes colnum
      let target = criteria col

      bytes
      |> List.filter (fun byte -> byte.[colnum] = target)
      |> filter (colnum + 1)

  System.Convert.ToInt32((filter 0 bytes).[0], 2)

let oxygen = filterBytes mostCommon
let co2 = filterBytes leastCommon

let part2 bytes = (oxygen bytes) * (co2 bytes)

part2 input
