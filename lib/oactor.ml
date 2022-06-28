(* Module Oactor *)

type 'a t = Actor of 'a
  constraint 'a = <scheduler : Multiroundrobin.t; domain : Domain.id; ..> [@@ocaml.unboxed]

let send (Actor actor) process =
  Multiroundrobin.push_process actor#scheduler process

let methods (Actor actor) = actor

let in_same_domain (Actor actor) =
  actor#domain = Domain.self ()

let forward p =
  Effect.perform @@ Multiroundrobin.Forward (fun fill -> Promise.add_callback p fill)
