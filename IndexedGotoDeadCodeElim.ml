open IndexedGotoLiveness

let step i =
  let l = liveness i in 
  match i with
  | Sequence(i1, i2) ->
  | Set(loc, e) ->

let dead_code_elim i =
  i
