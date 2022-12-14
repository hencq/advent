open System.IO

let input = File.ReadAllText "input6.txt"

let findMarker n (s : string) =
    let rec loop i =
        if i = (String.length s) then None
        else
            let seen =
                [ for j = max 0 (i - n) to i - 1 do s[j] ] |> Set.ofList
            if (Set.count seen) = n then Some i
            else loop (i + 1)
    loop 0 

findMarker 4 input
findMarker 14 input
