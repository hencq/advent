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

let rec explode (lst : LinkedList<Node>) (ln : LinkedListNode<Node>) =
  let add (an : LinkedListNode<Node>) (b : Node) =
    if an = null then
      ()
    else
      match an.Value, b with
      | Value av, Value bv ->
          printfn "%A + %A" av bv
          an.Value <- Value (av + bv)
          split lst an
          ()
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
    let lh = ln.Next.Value
    let left = scan (fun n -> n.Previous) ln
    let rh = ln.Next.Next.Value
    let right = scan (fun n -> n.Next) ln.Next.Next.Next

    printfn "Exploding [%A, %A]" lh rh
    // Remove 4 nodes: [ a b ]
    let ln' = lst.AddAfter(ln.Previous, (Value 0))
    lst.Remove(ln.Next.Next.Next)
    lst.Remove(ln.Next.Next)
    lst.Remove(ln.Next)
    lst.Remove(ln)

    add left lh
    add right rh
    printList lst
    ln'
  else
    ln  
  
and split (lst : LinkedList<Node>) (ln : LinkedListNode<Node>) : LinkedListNode<Node> =
  match ln.Value with
  | Value v when v > 9 ->
      let a = v / 2
      let b = a + v % 2
      let pn = lst.AddAfter(ln, Open)
      lst.AddAfter(ln.Next, Value a)
      lst.AddAfter(ln.Next.Next, Value b)
      lst.AddAfter(ln.Next.Next.Next, Close)
      lst.Remove(ln)
      printfn "Splitting %A into [%A, %A] depth: %A" v a b (depth pn) 
      printList lst
      explode lst pn
  | _ -> ln

let reduceSum s =
  let lst = LinkedList s
  let rec loop (ln : LinkedListNode<Node>) =
    if ln = null then
      lst |> List.ofSeq
    else
      loop (explode lst ln).Next
  loop lst.First

let addLists a b =
  let c = List.concat [[Open]; a; b; [Close]]
  c

let test = "[[[[[9,8],1],2],3],4]" |> makeList

reduceSum test

test |> List.ofSeq

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
|> Seq.take 2
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
