open System.IO
open System.Collections.Generic

type Node =
  | Open
  | Close
  | Value of int
  
let makeList input =
  input
  |> Seq.choose
       (fun ch ->
          match ch with
          | '[' -> Some Open
          | ']' -> Some Close
          | ',' -> None
          | ch -> Some (Value (int ch - int '0')))
  |> List.ofSeq

let depth (ln : LinkedListNode<Node>) =
  let rec loop (ln : LinkedListNode<Node>) depth =
    if ln = null then
      depth
    else
      match ln.Value with
      | Open -> loop ln.Previous (depth + 1)
      | Close -> loop ln.Previous (depth - 1)
      | Value _ -> loop ln.Previous depth

  match ln.Value with
  | Open -> loop ln -1
  | Value _ -> loop ln -1
  | Close -> loop ln 0

exception Error of string

let explode (lst : LinkedList<Node>) (ln : LinkedListNode<Node>) =
  let add (a : LinkedListNode<Node>) (b : LinkedListNode<Node>) =
    if a = null then
      ()
    else
      match a.Value, b.Value with
      | Value av, Value bv ->
          a.Value <- Value (av + bv)
      | _ -> raise (Error "trying to add 2 non-values")

  let rec scan dir (ln : LinkedListNode<Node>) =
    if ln = null then
      ln
    else
      match ln.Value with
      | Open -> scan dir (dir ln) 
      | Close -> scan dir (dir ln)
      | Value _ -> ln

  if (depth ln) >= 4 then
    let left = scan (fun n -> n.Previous) ln
    add left ln.Next
    let right = scan (fun n -> n.Next) ln.Next.Next.Next
    add right ln.Next.Next

    // Remove 4 nodes: [ a b ]
    let ln' = lst.AddAfter(ln.Previous, (Value 0))
    lst.Remove(ln.Next.Next.Next)
    lst.Remove(ln.Next.Next)
    lst.Remove(ln.Next)
    lst.Remove(ln)
    true
  else
    false  
  
let split (lst : LinkedList<Node>) (ln : LinkedListNode<Node>) =
  match ln.Value with
  | Value v when v > 9 ->
      let a = v / 2
      let b = a + v % 2
      let pn = lst.AddAfter(ln, Open)
      lst.AddAfter(ln.Next, Value a)
      lst.AddAfter(ln.Next.Next, Value b)
      lst.AddAfter(ln.Next.Next.Next, Close)
      lst.Remove(ln)
      true
  | _ -> false

let reduceSum s =
  let lst = LinkedList s
  let rec sweepExplode (ln : LinkedListNode<Node>) =
    if ln = null then
      sweepSplit lst.First
    elif explode lst ln then
      sweepExplode lst.First
    else
      sweepExplode ln.Next
  and sweepSplit (ln : LinkedListNode<Node>) =
    if ln = null then
      lst |> List.ofSeq
    elif split lst ln then
      sweepExplode lst.First
    else
      sweepSplit ln.Next
  sweepExplode lst.First

let addLists a b =
  let c = List.concat [[Open]; a; b; [Close]]
  reduceSum c

let test = "[[[[[9,8],1],2],3],4]" |> makeList

addLists (makeList "[[[[4,3],4],4],[7,[[8,4],9]]]") (makeList "[1,1]")
|> reduceSum

let readInput fname =
  File.ReadAllLines fname
  |> Seq.map makeList

readInput "test18.txt"
|> Seq.reduce (fun a b -> (addLists a b) |> reduceSum)

let test2 =
  [ "[1,1]"
    "[2,2]"
    "[3,3]"
    "[4,4]"
    "[5,5]"
    "[6,6]" ]
  |> List.map makeList
  |> List.reduce (fun a b -> addLists a b |> reduceSum)

readInput "test18-2.txt"
|> Seq.reduce (fun a b -> addLists a b |> reduceSum)
|> printList

let printList lst =
  lst
  |> Seq.iter
       (fun x ->
          match x with
          | Open -> printf "["
          | Close -> printf "]"
          | Value v -> printf "%A " v)
  printfn ""

let magnitude ls =
  let rec parse ls =
    match ls with
    | [] -> 0, []
    | hd::tail ->
        match hd with
        | Open ->
          let left, tail' = parse tail
          let right, tail'' = parse tail'
          (3 * left + 2 * right), (List.tail tail'')
        | Value v -> v, tail
        | Close -> raise (Error "unexpected ]")
  parse ls

readInput "test18.txt"
|> Seq.reduce addLists
|> magnitude

readInput "input18.txt"
|> Seq.reduce addLists
|> magnitude

let combinations xs =
  seq { for x in xs do
          for x' in xs do
            if x <> x' then
              yield (x, x') }

readInput "input18.txt"
|> combinations
|> Seq.map
     (fun (a, b) -> addLists a b |> magnitude)
|> Seq.max
