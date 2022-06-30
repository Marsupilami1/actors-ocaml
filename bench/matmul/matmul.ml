open Actorsocaml

type 'a computation_tree = Node of 'a Actor.t * 'a computation_tree list

let zip l l' = List.map2 (fun x y -> (x, y)) l l'

let split_matrix m1 =
  let size = Array.length m1 in
  let len = size / 2 in
  let p12 = Array.sub m1 0 len in
  let p1 = Array.map (fun t -> Array.sub t 0 len) p12 in
  let p2 = Array.map (fun t -> Array.sub t len len) p12 in
  let p34 = Array.sub m1 (size / 2) len in
  let p3 = Array.map (fun t -> Array.sub t 0 len) p34 in
  let p4 = Array.map (fun t -> Array.sub t len len) p34 in
  p1, p2, p3, p4

let merge_matrix res l =
  match l with
  | [p1; p2; p3; p4] ->
    let size = Array.length res in
    let len = size/2 in
    for i=0 to size-1 do
      for j=0 to size-1 do
        let src =
          if i < len then
            if j < len then p1.(i).(j) else p3.(i).(j - len)
          else
          if j < len then p2.(i - len).(j) else p4.(i - len).(j - len)
        in res.(i).(j) <- src
      done
    done
  | _ -> ()

let print_matrix m =
  let size = Array.length m in
  for i=0 to size - 1 do
    for j=0 to size - 1 do
      Printf.printf "%d " m.(i).(j)
    done;
    print_newline ()
  done

let (++) m1 m2 =
  for i=0 to Array.length m1 - 1 do
    for j=0 to Array.length m1.(i) - 1 do
      m1.(i).(j) <- m1.(i).(j) + m2.(i).(j)
    done
  done

let rec multiplicator n =
  object%actor
    val children = if n = 1 then [] else List.init 4 (fun _ -> multiplicator (n / 2))
    method compute (m1, m2) =
      let size = Array.length m1 in
      match size with
      | 1 -> m1.(0).(0) <- m1.(0).(0) * m2.(0).(0)
      | _ -> begin
          (* divide the matrix by four *)
          let p1, p2, p3, p4 = split_matrix m1 in
          let q1, q2, q3, q4 = split_matrix m2 in
          let ps = List.map2 (fun a -> a#.compute) children [p1, q1; p2, q4; p3, q1; p4, q4] in
          List.iter Promise.await ps;
          merge_matrix m2 [p1; p2; p3; p4];
          let p1, p2, p3, p4 = split_matrix m1 in
          let ps = List.map2 (fun a -> a#!compute) children [p2, q3; p1, q3; p4, q2; p3, q2] in
          List.iter Promise.await ps;
          merge_matrix m1 [p2; p1; p4; p3];
          m1 ++ m2
        end
  end

let random_matrix k n =
  let a = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      a.(i).(j) <- (i + k * j) mod 367
    done
  done;
  a

let main _ =
  let n = 32 in
  let m = multiplicator n in
  let m1 = random_matrix 17 n in
  let m2 = random_matrix 7 n in

  let f () = Promise.get (m#!compute (m1, m2)) in

  let r = 40 in
  let samples = Benchmark.latency1 ~name: "Mat Mul" (Int64.of_int r) f () in
  Benchmark.tabulate samples


let _ = Actor.Main.run main
