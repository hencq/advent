open System.IO

let roll die =
  die % 100 + 1

type Player =
  { Score : int
    Place : int }

let turn1 player die =
  let r1 = roll die
  let r2 = roll r1
  let r3 = roll r2
  let place = (player.Place + r1 + r2 + r3 - 1) % 10 + 1
  { Score = player.Score + place; Place = place }, r3

let turn p1 p2 die =
  let p1', die' = turn1 p1 die
  let p2', die'' = turn1 p2 die'
  p1', p2', die''
  
let part1 s1 s2 =
  let p1 = { Score = 0; Place = s1 }
  let p2 = { Score = 0; Place = s2 }
  let rec loop rolls p1 p2 die =
    let p1', die' = turn1 p1 die
    if p1'.Score >= 1000 then
      (rolls + 3) * p2.Score
    else
      let p2', die'' = turn1 p2 die'
      if p2'.Score >= 1000 then
        (rolls + 6) * p1'.Score
      else
        loop (rolls + 6) p1' p2' die''
  loop 0 p1 p2 0
  

part1 9 4

let rolls =
  [ for a in 1 .. 3 do
    for b in 1 .. 3 do
      for c in 1 .. 3 ->
        a + b + c ]
  |> List.countBy id
  |> List.map (fun (r, c) -> (r, uint64 c))


type Turn = bool
type GameState =
  { P1 : Player
    P2 : Player
    Turn : Turn }

let newState s1 s2 =
  { P1 = { Place = s1; Score = 0 }
    P2 = { Place = s2; Score = 0 }
    Turn = true }
    
let addRoll state roll =
  let p = if state.Turn then state.P1 else state.P2
  let place = (p.Place + roll - 1) % 10 + 1
  let p' = { Place = place; Score = p.Score + place }
  if state.Turn then
    { state with P1 = p'; Turn = not state.Turn }
  else
    { state with P2 = p'; Turn = not state.Turn }

let isDone state =
  state.P1.Score >= 21 || state.P2.Score >= 21

let groupStates (states : (GameState * uint64) list) =
  states
  |> List.groupBy fst
  |> List.map (fun (st, sts) ->
                 sts
                 |> List.map snd
                 |> List.sum
                 |> uint64
                 |> fun sum -> (st, sum))
                 
let nextStates (state, (count : uint64)) rolls =
  rolls
  |> List.map (fun (r, c) -> (addRoll state r, c * count))

let rec playGame (states : (GameState * uint64) list) =

  let states' =
    states
    |> List.collect (fun (s, c) ->
         if isDone s then [ (s, c) ] else nextStates (s, c) rolls)
    |> groupStates
  if List.forall (fst >> isDone) states' then
    states'
  else
    playGame states'
    
let part2 s1 s2 =
  playGame [ (newState s1 s2, 1UL) ]
  |> List.fold
       (fun (s1, s2) (st, c) ->
          if st.P1.Score >= 21 then
            (s1 + c, s2)
          else
            (s1, s2 + c))
       (0UL, 0UL)
  ||> max
 
part2 4 8
part2 9 4
