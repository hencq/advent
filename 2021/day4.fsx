open System.IO

type Position = int * int
type BingoCard = {
  Remaining : Map<int, Position>
  Mask : bool array array 
}

let readCard (block : string) =
  let remain =
    block
    |> fun b -> b.Split("\n")
    |> Array.indexed
    |> Array.collect (fun (y, l) ->
                        l.Trim().Split(" ")
                        |> Array.mapi (fun x n -> (int n, (x, y)))) 
    |> Map.ofArray
  let mask = Array.init 5 (fun _ -> (Array.create 5 false))
  {Remaining = remain; Mask = mask}
                
                     
let readData fname =
  let numbers, blocks =
    File.ReadAllText fname
    |> fun txt -> txt.Trim().Replace("  ", " ").Split("\n\n")
    |> fun blocks ->
      let nums =
        Array.head blocks
        |> fun l -> l.Split(",")
        |> List.ofArray
        |> List.map int
      (nums, Array.tail blocks)
  let cards =
    blocks
    |> List.ofArray
    |> List.map readCard
  (numbers, cards)
  
let callNumber num (card : BingoCard) =
  match card.Remaining.TryFind num with
  | Some (x, y) ->
    card.Mask.[y].[x] <- true
    {card with Remaining = card.Remaining.Remove num}
  | None -> card
  
let hasBingo (card : BingoCard) =
  let rows = Seq.ofArray card.Mask
  rows
  |> Array.transpose
  |> Seq.ofArray
  |> Seq.append rows
  |> Seq.exists (fun s -> Array.forall id s)
let test = readData "test4.txt"

let nums, cards = test
let card1 = List.head cards 
let card3 = cards.[2]
hasBingo card1

let rec runBingo cards nums =
   if List.isEmpty nums then
     None
   else
     let num = List.head nums
     let newcards = cards |> List.map (fun c -> callNumber num c) 
     match newcards |> List.tryFind hasBingo with
     | Some card -> Some (num, card)
     | None -> runBingo newcards (List.tail nums)

let findCard finder (fname : string) =
  let nums, cards = readData fname
  match finder cards nums with
  | None -> None
  | Some (num, card) -> Some (
    card.Remaining
    |> Map.keys
    |> Seq.sum
    |> fun s -> s * num)

let part1 = findCard runBingo
part1 "test4.txt"
part1 "input4.txt"

let rec findLastCard cards nums =
  if List.isEmpty nums then
    None
  else
    let num = List.head nums
    let newcards = cards |> List.map (fun c -> callNumber num c)
    match newcards |> List.filter (fun c -> not (hasBingo c)) with
      | [] -> None
      | [c] -> runBingo [c] (List.tail nums)
      | _ -> findLastCard newcards (List.tail nums)

let part2 = findCard findLastCard
part2 "test4.txt"
part2 "input4.txt"
