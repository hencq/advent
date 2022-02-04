open System.Collections.Generic
 
type Amph =
  | A
  | B
  | C
  | D

let cost =
  [ (A, 1); (B, 10); (C, 100); (D, 1000) ] |> Map.ofList
type Tile =
  | Rest
  | Hall
  | Room of Amph
  
type Square =
  | Empty
  | Taken of Amph
  
type State = Square array

let isTaken (state : State) i =
  match state.[i] with
  | Empty -> false
  | Taken _ -> true
  
type Plan = (Tile * int list) array

let plan =
  [| (Rest, [1]);
     (Rest, [0; 2]);
     (Hall, [1; 3; 12]);
     (Rest, [2; 4]);
     (Hall, [3; 5; 14]);
     (Rest, [4; 6]);
     (Hall, [5; 7; 16]);
     (Rest, [6; 8]);
     (Hall, [7; 9; 18]);
     (Rest, [8; 10]);
     (Rest, [9]);
     (Room A, [12]);
     (Room A, [11; 2]);
     (Room B, [14]);
     (Room B, [13; 4]);
     (Room C, [16]);
     (Room C, [15; 6]);
     (Room D, [18]);
     (Room D, [17; 8]) |]

let input = Array.create 19 Empty
input.[11] <- Taken D
input.[12] <- Taken B
input.[13] <- Taken A
input.[14] <- Taken A
input.[15] <- Taken D
input.[16] <- Taken B
input.[17] <- Taken C
input.[18] <- Taken C

let dist (plan : Plan) (state : State) start dest =
  let q = new Queue<(int * int)>()
  q.Enqueue((start, 0))
  let seen = new HashSet<int>()
  let rec loop () =
    if q.Count = 0 then
      None
    else
      let (sq, len) = q.Dequeue()
      if sq = dest then
        Some len
      elif not (seen.Contains(sq)) then
        plan.[sq]
        |> snd
        |> List.filter (isTaken state >> not)
        |> List.filter (fun x -> not (seen.Contains(x)))
        |> List.iter (fun adj -> q.Enqueue((adj, len + 1)))
        seen.Add(sq) |> ignore
        loop () 
      else
        loop ()
  loop ()

let isTarget (plan : Plan) amph i =
  match plan.[i] with
  | Room a, _ -> a = amph
  | _ -> false
  
let getTarget (plan : Plan) (state : State) amph =
  state
  |> Seq.indexed
  |> Seq.tryFind (fun (i, sq) ->
                    match plan.[i] with
                    | Room a, _ when a = amph ->
                      match sq with
                      | Taken b -> not (a = b)
                      | Empty -> true
                    | _ -> false)
  |> function
     | Some (i, _) -> Some i
     | _ -> None

let getRests (plan : Plan) =
  plan
  |> Seq.indexed
  |> Seq.choose (function
                 | i, (Rest, _) -> Some i
                 | _ -> None)
  |> Seq.toList
  
let destinations (plan : Plan) (state : State) i =
  match state.[i], (fst plan.[i]) with
  | Taken a, Room b when isHome plan state i -> []
  | Taken a, Rest -> [ getTarget plan state a ] |> List.choose id
  | Taken a, Room b ->
    match getTarget plan state a with
    | Some i -> i :: (getRests plan)
    | _ -> getRests plan
  | _ -> failwith "Unexpected state"
  
let rec isHome (plan : Plan) (state: State) i =
  match state.[i], (fst plan.[i]) with
  | Taken a, Room b when a = b ->
    match (fst plan.[i - 1]) with
    | Room c when a = c -> isHome plan state (i - 1)
    | _ -> true
  | _ -> false
  
let inPlay (plan : Plan) (state : State) =
  [ for i in 0 .. (Array.length state) - 1 -> i ]
  |> List.filter (fun i ->
                    match state.[i] with
                    | Taken _ -> not (isHome plan state i)
                    | _ -> false)
                     
getTarget plan input D
isTarget plan A 1
destinations plan input 12

inPlay plan input

let move (plan : Plan) (state : State) start dest =
  match dist plan state start dest with
  | Some delta ->
    let amph =
      match state.[start] with
      | Taken a -> a
      | _ -> failwith "Can only move a taken square"
              
    let state' = Array.copy state
    state'.[dest] <- Taken amph
    state'.[start] <- Empty
    Some (cost.[amph] * delta, state')
  | _ -> None
  
let nextStates (plan : Plan) (state : State) =
  let pods = inPlay plan state
  [ for p in pods do
      for d in destinations plan state p ->
        (p, d) ]
  |> List.choose (fun (p, d) -> (move plan state p d))

nextStates plan input
dist plan input 12 0

let solve (plan : Plan) (state : State) =
  let q = new PriorityQueue<State * int, int>()
  q.Enqueue((state, 0), 0)
  let rec loop seen =
    if q.Count = 0 then
      failwith "No solution found"
    else
      let (state, cost) = q.Dequeue()
      let players = inPlay plan state
      if (List.length players) = 0 then
        cost
      elif Set.contains state seen then
        loop seen 
      else
        nextStates plan state
        |> List.filter (fun (_, adj) -> Set.contains adj seen |> not)
        |> List.iter (fun (c, adj) ->
                        let cost' = cost + c
                        q.Enqueue((adj, cost'), cost'))
        loop (Set.add state seen)
  loop Set.empty 
                        
        
solve plan input     

let plan2 =
  [| (Rest, [1]);
     (Rest, [0; 2]);
     (Hall, [1; 3; 14]);
     (Rest, [2; 4]);
     (Hall, [3; 5; 18]);
     (Rest, [4; 6]);
     (Hall, [5; 7; 22]);
     (Rest, [6; 8]);
     (Hall, [7; 9; 26]);
     (Rest, [8; 10]);
     (Rest, [9]);
     (Room A, [12]);
     (Room A, [11; 13]);
     (Room A, [12; 14]);
     (Room A, [13; 2]);
     (Room B, [16]);
     (Room B, [15; 17]);
     (Room B, [16; 18]);
     (Room B, [17; 4]);
     (Room C, [20]);
     (Room C, [19; 21]);
     (Room C, [20; 22]);
     (Room C, [21; 6]);
     (Room D, [24]);
     (Room D, [23; 25]);
     (Room D, [24; 26]);
     (Room D, [25; 8]) |]

let input2 = Array.create 27 Empty
input2.[11] <- Taken D
input2.[12] <- Taken D
input2.[13] <- Taken D
input2.[14] <- Taken B

input2.[15] <- Taken A
input2.[16] <- Taken B
input2.[17] <- Taken C
input2.[18] <- Taken A

input2.[19] <- Taken D
input2.[20] <- Taken A
input2.[21] <- Taken B
input2.[22] <- Taken B

input2.[23] <- Taken C
input2.[24] <- Taken C
input2.[25] <- Taken A
input2.[26] <- Taken C

solve plan2 input2

