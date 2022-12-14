open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

type Directory = { Parent : Directory option;
                   Contents : Dictionary<string, Tree>;
                   mutable Size : int }
and Tree =
    | Directory of Directory
    | File of int

let newDir parent =
    { Parent = parent; Contents = new Dictionary<string, Tree>(); Size = 0 }

let addFile name size dir =
    dir.Contents.Add(name, File size)
    let rec loop dir =
        dir.Size <- dir.Size + size
        match dir.Parent with
        | Some p -> loop p
        | None -> ()
    loop dir
    dir

let addDir name parent =
    parent.Contents.Add(name, Directory (newDir (Some parent)))
    parent

let cd name dir =
    match dir.Contents.TryGetValue name with
    | true, Directory v -> Some v
    | _ -> None

let up dir =
    dir.Parent

let rec root dir =
    match dir.Parent with
    | Some p -> root p
    | None -> dir
    
newDir None
|> addFile "foo" 53
|> addDir "bar"
|> cd "bar"
|> fun x -> match x with | Some dir -> up dir


let (|Regex|_|) pattern str =
    let m = Regex.Match(str, pattern)
    if m.Success then
        [ for g in m.Groups -> g.Value ]
        |> List.tail
        |> List.toArray
        |> Some
    else None
    
let parseRow tree row =
    match row with
    | Regex "\$ cd (.+)" groups ->
        if groups[0] = "/" then root tree
        elif groups[0] = ".." then
            match up tree with
            | Some dir -> dir
            | None -> tree
        else
            match cd groups[0] tree with
            | Some dir -> dir
            | None -> tree
    | Regex "(\d+) ([\w\.]+)" groups -> tree |> addFile groups[1] (int groups[0])
    | Regex "dir (\w+)" groups -> tree |> addDir groups[0]
    | _ -> tree
        
let rec traverse tree =
    seq {
        match tree with
        | File _ as f -> yield f
        | Directory d as t -> yield t
                              for c in d.Contents.Values do yield! traverse c  
        }

let part1 fname =
    File.ReadAllLines fname
    |> Array.fold parseRow (newDir None)
    |> root
    |> Directory
    |> traverse
    |> Seq.choose (function |Directory d -> Some d |_ -> None)
    |> Seq.map (fun x -> x.Size)
    |> Seq.filter (fun x -> x <= 100000)
    |> Seq.sum

part1 "input7.txt"

let part2 fname =
    let root =
        File.ReadAllLines fname
        |> Array.fold parseRow (newDir None)
        |> root
    let needed = 30000000 - (70000000 - root.Size)
    root
    |> Directory
    |> traverse
    |> Seq.choose (function |Directory d -> Some d.Size |_ -> None)
    |> Seq.sort
    |> Seq.filter (fun x -> x >= needed)
    |> Seq.head

part2 "input7.txt"
    
