let f (x[@resolve]) = x
;;
let f (x[@resolve]) y =
  let x = List.hd x + y in
  x
;;
