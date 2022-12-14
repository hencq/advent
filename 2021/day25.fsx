open System
open System.IO

let readMap fname =
  File.ReadAllLines fname
  |> array2D

let test = readMap "test25.txt"
let test2 = Array2D.copy test

let moveRight (src : char[,]) (dst : char[,]) i j =
  let cols = Array2D.length2 src
  if src.[i, j] = '>' && src.[i, (j + 1) % cols] = '.' then
    dst.[i, j] <- '.'
    dst.[i, (j + 1) % cols] <- '>'
    1
  else
    0

let moveDown (src : char[,]) (dst : char[,]) i j =
  let rows = Array2D.length1 src
  if src.[i, j] = 'v' && src.[(i + 1) % rows, j] = '.' then
    dst.[i, j] <- '.'
    dst.[(i + 1) % rows, j] <- 'v'
    1
  else
    0
  
let copyArray mover (src : char[,]) (dst : char[,]) =
  let rows = Array2D.length1 src
  let cols = Array2D.length2 src
  dst.[0..rows-1, 0..cols-1] <- src
  let mutable moves = 0
  for i in 0 .. rows - 1 do
    for j in 0 .. cols - 1 do
      moves <- moves + (mover src dst i j)
  moves

let step src dst =
  (copyArray moveRight src dst) + (copyArray moveDown dst src) > 0

let part1 input =
  let src = Array2D.copy input
  let dst = Array2D.copy input
  let mutable steps = 1
  while (step src dst) do
    steps <- steps + 1
  steps
  
let input = readMap "input25.txt"

part1 input
part1 test
