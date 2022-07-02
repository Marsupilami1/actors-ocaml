open Actorsocaml
open Bigarray

let matmul m1 i1 k1 m2 k2 j2 res ir jr len =
  for i=0 to len-1 do
    for j=0 to len-1 do
      for k=0 to len-1 do
        res.{ir + i, jr + j} <-
          res.{ir + i, jr + j} +. m1.{i1 + i, k1 + k} *. m2.{k2 + k, j2 + j}
      done
    done
  done

let print_matrix m =
  let size = Array2.dim1 m in
  for i=0 to size - 1 do
    for j=0 to size - 1 do
      Printf.printf "%f " @@ m.{i, j}
    done;
    print_newline ()
  done

let rec multiplicator len =
  if len <= 128 then
    object%actor
      method compute m1 i1 k1 m2 k2 j2 res ir jr =
        matmul m1 i1 k1 m2 k2 j2 res ir jr len
    end
  else
    object%actor
      val a1 = multiplicator (len/2)
      val a2 = multiplicator (len/2)
      val a3 = multiplicator (len/2)
      val a4 = multiplicator (len/2)
      method compute m1 i1 k1 m2 k2 j2 res ir jr =
        let hlen = len / 2 in
        (* Not very nice but it works *)
        let p1 = a1#!compute m1 i1 k1 m2 k2 j2 res ir jr    in
        let p2 = a2#!compute m1 i1 (k1 + hlen) m2 (k2 + hlen) (j2 + hlen) res ir (jr + hlen) in
        let p3 = a3#!compute m1 (i1 + hlen) k1 m2 k2 j2 res (ir + hlen) jr in
        let p4 = a4#!compute m1 (i1 + hlen) (k1 + hlen) m2 (k2 + hlen) (j2 + hlen) res (ir + hlen) (jr + hlen) in
        let p5 = a1#!compute m1 i1 (k1 + hlen) m2 (k2 + hlen) j2 res ir jr    in
        let p6 = a2#!compute m1 i1 k1 m2 k2 (j2 + hlen) res ir (jr + hlen) in
        let p7 = a3#!compute m1 (i1 + hlen) k1 m2 (k2 + hlen) j2 res (ir + hlen) jr in
        let p8 = a4#!compute m1 (i1 + hlen) (k1 + hlen) m2 k2 (j2 + hlen) res (ir + hlen) (jr + hlen) in

        Promise.await p1; Promise.await p2; Promise.await p3; Promise.await p4;
        Promise.await p5; Promise.await p6; Promise.await p7; Promise.await p8;
    end

let random_matrix n =
  Array2.init Float64 C_layout n n (fun _ _ -> 1. -. (Random.float 2.0))

let zero n =
  Array2.create Float64 C_layout n n

let main _ =
  let n = 258 in
  Random.init 42;
  let m1 = random_matrix n in
  let m2 = random_matrix n in
  let res = zero n in

  let multiplier = multiplicator n in

  let f () = multiplier#.compute m1 0 0 m2 0 0 res 0 0 in

  let r = 5 in
  let samples = Benchmark.latency1 ~name: "Mat Mul" (Int64.of_int r) f () in
  Benchmark.tabulate samples


let _ = Actor.Main.run main
