(** The type of actors *)
type 'a t = Actor of 'a
  constraint 'a = <scheduler : Multiroundrobin.t; domain : Domain.id; ..> [@@ocaml.unboxed]

(** [send actor process] tells [actor] to execute [process ()]. *)
val send : 'a t -> Multiroundrobin.process -> unit

(** [methods actor] returns the methods of [actor]. *)
val methods : 'a t -> 'a

(** [in_same_domain actor] tells if you're running in [actor]'s domain. *)
val in_same_domain : 'a t -> bool

(** [forward p] will fill the current promise with the result of [p]. *)
val forward : 'a Promise.t -> 'a



(** The [Main] Actor, which runs the [main] function. *)
module Main : sig
  (** Calling [run main] is mandatory in your programs. *)
  val run : (unit -> unit) -> unit
end
