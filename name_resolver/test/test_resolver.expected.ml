let f ((x)[@defined "x_270"]) = ((x)[@resolved "x_270"])
let f ((x)[@defined "x_272"]) y =
  let x = (List.hd ((x)[@resolved "x_272"])) + y in x
let f ((x)[@defined "x_339"]) =
  object val y = ((x)[@resolved "x_339"]) method foo = y end
;;let ((x)[@defined "x_349"]) = 42 in
  fun _ -> ((x)[@resolved "x_349"]) + ((x)[@resolved "x_349"])
