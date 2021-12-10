open System.IO

let test =
  [ "[({(<(())[]>[[{[]{<()<>>"
    "[(()[<>])]({[<{<<[]>>("
    "{([(<{}[<>[]}>{[]{[(<()>"
    "(((({<>}<{<{<>}{[]{[]{}"
    "[[<[([]))<([[{}[[()]]]"
    "[{[{({}]{}}([{[{{{}}([]"
    "{<[[]]>}<{[{[{[]{()[[[]"
    "[<(<(<(<{}))><([]([]()"
    "<{([([[(<>()){}]>(<<{{"
    "<{([{{}}[<[[[<>{}]]]>[]]" ]

let input = File.ReadAllLines "input10.txt"

type ParseState =
  | Stack of char list
  | Complete
  | Corrupt of char

let pairs =
  Map [ ('(', ')')
        ('{', '}')
        ('[', ']')
        ('<', '>') ]

let (|Opener|_|) token =
  if Map.containsKey token pairs then
    Some token
  else
    None

let (|Closer|_|) token =
  match token with
  | Opener _ -> None
  | _ -> Some token

let rec parse state line =
  match (state, line) with
  | (Stack [], []) -> Complete
  | (Stack s, []) -> Stack s
  | (Stack (o :: _), (Closer t) :: _) when pairs.[o] <> t -> Corrupt t
  | (Stack (o :: os), (Closer t) :: ts) -> parse (Stack os) ts
  | (Stack os, (Opener t) :: ts) -> parse (Stack(t :: os)) ts
  | (result, _) -> result

let parseLine line = line |> List.ofSeq |> parse (Stack [])

let score result =
  match result with
  | Corrupt ')' -> 3
  | Corrupt ']' -> 57
  | Corrupt '}' -> 1197
  | Corrupt '>' -> 25137
  | _ -> 0

let part1 lines =
  lines
  |> Seq.map parseLine
  |> Seq.map score
  |> Seq.sum

part1 input

let score2 result =
  (0L, result)
  ||> Seq.fold
        (fun score tag ->
          5L * score
          + match tag with
            | '(' -> 1L
            | '[' -> 2L
            | '{' -> 3L
            | '<' -> 4L)

let part2 input =
  input
  |> Seq.map parseLine
  |> List.ofSeq
  |> Seq.choose
       (function
       | Stack s -> Some s
       | _ -> None)
  |> Seq.map score2
  |> Seq.sort
  |> fun xs -> Seq.item ((Seq.length xs) / 2) xs

part2 input
