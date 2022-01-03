open System
open System.IO
open System.Globalization
open System.Collections



let chunk n str =
  let pad str = str + (String.replicate (n - (String.length str)) "0")
  let rec loop str accum =
    match n < (String.length str) with
    | true -> loop str[n..] (str[..n-1] :: accum)
    | false -> (pad str) :: accum
  loop str []

let parseHex (str : string) =
  chunk 8 (str.Trim())
  |> List.map (fun s -> Int32.Parse(s, NumberStyles.HexNumber))
  |> List.toArray
  |> BitArray


type ParseState =
  { Bits : BitArray
    Pos : int }

let newParser str =
  let bits = parseHex str
  { Bits = bits
    Pos = bits.Length - 1 }

let peek (ps : ParseState) =
  ps.Bits.[ps.Pos], {ps with Pos = ps.Pos - 1}

let test1 = newParser "D2FE28"

let parseBits n (ps : ParseState) =
  let rec loop n ps sum =
    if n = 0 then sum, ps
    else
      let b, ps = peek ps
      match b with
      | true -> loop (n - 1) ps (2UL * sum + 1UL)
      | false -> loop (n - 1) ps (2UL * sum)
  loop n ps 0UL
    
let parseGroups (ps : ParseState) =
  let rec loop ps sum =
    let flag, ps = peek ps
    let num, ps = parseBits 4 ps
    let sum = 16UL * sum + num
    match flag with
    | true -> loop ps sum
    | false -> sum, ps
  loop ps 0UL
  

type Packet =
  | Literal of Literal
  | Op of Op

and Literal =
  { Ver : int
    Id : int
    Value : uint64 }

and Op =
  { Ver : int
    Id : int
    Subs : Packet list }


 
exception ParseException of string

let parseLiteral ver (ps : ParseState) =
  let value, ps = parseGroups ps
  Literal { Ver = ver; Id = 4; Value = value }, ps

let rec parsePacket (ps : ParseState) =
  let ver, ps = parseBits 3 ps
  let idfr, ps = parseBits 3 ps
  let ver, idfr = int(ver), int(idfr)
  if idfr = 4 then
    parseLiteral ver ps
  else
    parseOp ver idfr ps
    
and parseSubsByLength len (ps : ParseState) =
  let goal = ps.Pos - len
  let rec loop ps accum =
    if ps.Pos = goal then
      (List.rev accum), ps
    else
      let sub, ps = parsePacket ps
      loop ps (sub :: accum)
  loop ps []
  
and parseSubsN n (ps : ParseState) =
  let rec loop n ps accum =
    if n = 0 then
      (List.rev accum), ps
    else
      let sub, ps = parsePacket ps
      loop (n - 1) ps (sub :: accum)
  loop n ps []
  
and parseOp ver idfr (ps : ParseState) =
  let len, ps = peek ps
  if len then
    let n, ps = parseBits 11 ps
    let n = int(n)
    let subs, ps = parseSubsN n ps
    (Op { Ver = ver; Id = idfr; Subs = subs }), ps
  else
    let len, ps = parseBits 15 ps
    let len = int(len)
    let subs, ps = parseSubsByLength len ps
    (Op { Ver = ver; Id = idfr; Subs = subs }), ps

  
let rec addVersions packet =
  match packet with
  | Literal p -> p.Ver
  | Op p -> p.Ver + (p.Subs |> List.map addVersions |> List.sum)
    
let part1 str =
  str
  |> newParser
  |> parsePacket
  |> fst
  |> addVersions
  
part1 "A0016C880162017C3686B18A3D4780"
part1 "C0015000016115A2E0802F182340"

let input = File.ReadAllText "input16.txt"
part1 input

let rec calcPacket (packet : Packet) =
  match packet with
  | Literal p -> p.Value
  | Op p ->
    match p.Id with
    | 0 -> p.Subs |> List.map calcPacket |> List.sum
    | 1 -> p.Subs |> List.map calcPacket |> List.reduce (*)
    | 2 -> p.Subs |> List.map calcPacket |> List.reduce min
    | 3 -> p.Subs |> List.map calcPacket |> List.reduce max
    | 5 -> p.Subs
           |> List.map calcPacket
           |> fun (a :: b :: []) -> if a > b then 1UL else 0UL
    | 6 -> p.Subs
           |> List.map calcPacket
           |> fun (a :: b :: []) -> if a < b then 1UL else 0UL
    | 7 -> p.Subs
           |> List.map calcPacket
           |> fun (a :: b :: []) -> if a = b then 1UL else 0UL
    | _ -> raise (ParseException "Unexpected op")

let part2 str =
  str
  |> newParser
  |> parsePacket
  |> fst
  |> calcPacket

"9C0141080250320F1802104A08" |> part2
part2 "C200B40A82"

part2 input
