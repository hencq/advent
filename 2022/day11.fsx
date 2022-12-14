open System.IO
open System.Collections.Generic

type Monkey = { items : Queue<bigint>; op : bigint -> bigint; test : bigint -> int; mutable count : int }

let monkey (items : int seq) op (test : int) t f =
    let items = items |> Seq.map bigint
    { items = new Queue<_>(items);
      op = op;
      test = (fun i -> if i % (bigint test) = (bigint 0) then t else f)
      count = 0 }

let doMonkey adjust i (monkies : Monkey[]) =
    while monkies[i].items.Count > 0 do
        let item = monkies[i].items.Dequeue() |> monkies[i].op |> adjust
        let dst = monkies[i].test item
        monkies[dst].items.Enqueue(item)
        monkies[i].count <- monkies[i].count + 1
    monkies

let playRounds adjust n monkies =
    for r = 1 to n do 
        for m = 0 to (Array.length monkies) - 1 do
            doMonkey adjust m monkies |> ignore
    monkies

     
let part1 monkies =
    monkies
    |> playRounds (fun i -> i/(bigint 3)) 20
    |> Array.sortByDescending (fun m -> m.count)
    |> fun monkies -> monkies[0].count * monkies[1].count

let input =
    [| monkey [92; 73; 86; 83; 65; 51; 55; 93] ((*) (bigint 5)) 11 3 4;
       monkey [99; 67; 62; 61; 59; 98] (fun i -> i * i) 2 6 7;
       monkey [81; 89; 56; 61; 99] ((*) (bigint 7)) 5 1 5;
       monkey [97; 74; 68] ((+) (bigint 1)) 17 2 5;
       monkey [78; 73] ((+) (bigint 3)) 19 2 3;
       monkey [50] ((+) (bigint 5)) 7 1 6;
       monkey [95; 88; 53; 75] ((+) (bigint 8)) 3 0 7;
       monkey [50; 77; 98; 85; 94; 56; 89] ((+) (bigint 2)) 13 4 0
    |]

part2 input



let test =
    [| monkey [79; 98] ((*) (bigint 19)) 23 2 3;
       monkey [54; 65; 75; 74] ((+) (bigint 6)) 19 2 0;
       monkey [79; 60; 97] (fun i -> i * i) 13 1 3;
       monkey [74] ((+) (bigint 3)) 17 0 1
    |]

[| monkey [79; 98] ((*) (bigint 19)) 23 2 3;
   monkey [54; 65; 75; 74] ((+) (bigint 6)) 19 2 0;
   monkey [79; 60; 97] (fun i -> i * i) 13 1 3;
   monkey [74] ((+) (bigint 3)) 17 0 1
|]
|> playRounds (fun x -> x % (bigint 96577)) 3000 |> Seq.map (fun m -> m.count)

let part2 monkies =
    let divisor = 11 * 2 * 5 * 17 * 19 * 7 * 3 * 13
    let f x = x % (bigint divisor)
    monkies
    |> playRounds f 10000
    |> Array.sortByDescending (fun m -> m.count)
    |> fun monkies -> (bigint monkies[0].count) * (bigint monkies[1].count)


let experiment start n =
    let diffs arr1 arr2 =
        Array.map2 (-) arr1 arr2
    let rec loop i (last : int[]) =
        if i = n then last
        else
            let value = 
                [| monkey [92; 73; 86; 83; 65; 51; 55; 93] ((*) (bigint 5)) 11 3 4;
                   monkey [99; 67; 62; 61; 59; 98] (fun i -> i * i) 2 6 7;
                   monkey [81; 89; 56; 61; 99] ((*) (bigint 7)) 5 1 5;
                   monkey [97; 74; 68] ((+) (bigint 1)) 17 2 5;
                   monkey [78; 73] ((+) (bigint 3)) 19 2 3;
                   monkey [50] ((+) (bigint 5)) 7 1 6;
                   monkey [95; 88; 53; 75] ((+) (bigint 8)) 3 0 7;
                   monkey [50; 77; 98; 85; 94; 56; 89] ((+) (bigint 2)) 13 4 0
                |]
                |> playRounds id (start + i)
                |> Seq.map (fun m -> m.items.Count)
                |> Seq.toArray
                |> fun x -> printfn "%A" x; x
            loop (i + 1) value
    loop 0 (Array.create 8 0)

experiment 130 40
