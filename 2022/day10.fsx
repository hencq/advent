open System.IO
open System.Text.RegularExpressions

type Instr = |Noop |Addx of int

let (|Regex|_|) pat str =
   let m = Regex.Match(str, pat)
   if m.Success then
       [ for g in m.Groups do g.Value ]
       |> Seq.tail
       |> Seq.toArray
       |> Some
   else None

let readInput fname =
    File.ReadAllLines fname
    |> Array.collect (function
                      | Regex "noop" _ -> [| Noop |]
                      | Regex "addx (-?\d+)" groups -> [| Noop; Addx (int groups[0]) |]
                      | _ -> failwith "Unknown instruction" )

let test = readInput "test10.txt"
let input = readInput "input10.txt"

let compute (sum, x) (i, instr) = 
    let x' =
        match instr with
        | Noop -> x
        | Addx n ->
            x + n
    let sum' = if i = 20 || (i - 20) % 40 = 0 then
                   sum + i * x
               else
                   sum
    (sum', x')

let draw x (i, instr) =
    let x' =
        match instr with
        | Noop -> x
        | Addx n -> x + n
    let i' = (i - 1) % 40 
    if x >= (i' - 1)  && x <= (i' + 1) then printf "#" else printf "."
    if i % 40 = 0 then printfn ""
    x'
    
let part1 input =
    input
    |> Array.mapi (fun i x -> (i + 1, x))
    |> Array.fold compute (0, 1)

let part2 input =
    printfn ""
    input
    |> Array.mapi (fun i x -> (i + 1, x))
    |> Array.fold draw 1

part1 input
part2 input
