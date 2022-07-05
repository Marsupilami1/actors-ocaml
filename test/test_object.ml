open Actorsocaml

(* let foo = object (this) *)
(*   method private field = 0 *)
(*   method foo = *)
(*     let self = this in *)
(*     self#field *)
(* end *)

let a = object%actor
  method greetings =
    print_endline "hello"
end

let split x xs =
  let worker (l, g) e =
    if e < x then (e :: l, g) else (l, e :: g)
  in List.fold_left worker ([], []) xs

let rec qsort l =
  match l with
  | [] -> []
  | x::xs ->
    let l, g = split x xs in
    (qsort l) @ (x :: qsort g)

let rec mk_sorter () =
  object%actor
    method sort l =
      match l with
      | [] -> []
      | _ when List.length l < 100000 -> qsort l
      | x::xs ->
        let l, g = split x xs in
        let sortl = mk_sorter () in
        let sortg = mk_sorter () in
        let pl = sortl#!sort l in
        let pg = sortg#!sort g in
        (Promise.await pl) @ (x :: (Promise.await pg))
  end


let main () =
  let a = object%actor
    val mutable y = 0
    method get = y
    method set n = y <- n
  end in

  let b = object%actor
    method get = a#!!get
  end in
  a#.set 42;
  let p = b#!get in
  Printf.printf "a.y = %d\n"  @@ (Promise.get p);


  let l = List.init 100_000 (fun n -> 47 * n mod 367) in
  let sorter = mk_sorter () in
  print_endline "start";
  let _sorted_l = sorter#.sort l in
  print_endline "ok: sorted"



(* Printf.printf "foo = %d\n"  @@ foo#field *)

let _ = Actor.Main.run main
