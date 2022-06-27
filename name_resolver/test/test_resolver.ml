let f (x[@resolve]) = x
;;
let f (x[@resolve]) y =
  let x = List.hd x + y in
  x
;;
let f (x[@resolve]) =
  object
    val y = x
    method foo = y
  end
;;
let (x[@resolve]) = 42 in
  fun _ -> x + x
;;
