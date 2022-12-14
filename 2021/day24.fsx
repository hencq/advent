
let step z (div dx dy inp) =
  let mutable x = z mod 26
  let z' = z / div
  x <- x + dx
  if x = inp then
    z'
  else
    z' * 26 + inp + dy

step 0 (1, 12, 7, 1)
    
