(** Promises to be fulfilled.

    This module is used by the [Actor] module
    *)

(** {1 Types, Exceptions and Effects} *)

(** The type of promises. *)
type 'a t

(** Effect raised by the {!await} function. *)
type _ Effect.t += NotReady : 'a t -> 'a Effect.t

(** Raised when multiple fulfillment occur on the same promise *)
exception Promise__Multiple_Write

(** {1 Creation and access} *)

(** [create ()] creates an empty promise, it returns the
    promise and a [fill] function *)
val create : unit -> 'a t * ('a -> unit)

(** [pure v] creates a promise with the value [v]
    directly available. *)
val pure : 'a -> 'a t

(** [await p] raise the Effect ({!NotReady} [p])
    if [p] is empty, and return the value if
    [p] is fulfilled. *)
val await : 'a t -> 'a

(** [get p] blocks until the result of [p] is available, then it returns the value. *)
val get : 'a t -> 'a

(** [is_ready p] checks if the value of [p] is available. *)
val is_ready : 'a t -> bool

(** [add_callback p f] will add the callback [f] to the
    promise [p]. When [p] will be fulfilled with a value
    [v], the promise will call [f v].
    If the promise is already fulfilled when [add_callback]
    is called, [f v] is called directly. *)
val add_callback : 'a t -> ('a -> unit) -> unit


(** {1 Useful functions} *)

(** [unify p p'] unifies two promises. They now share
    their callbacks and will be fulfilled at the same time. *)
val unify : 'a t -> 'a t -> unit

(** [fmap f p] is the classic [fmap] function, it maps the
    function [f] into the promise [p]. *)
val fmap : ('a -> 'b) -> 'a t -> 'b t

(** [join p] make a promise from a promise of promise. *)
val join : 'a t t -> 'a t

(** [bind m f] is the classic monadic bind function. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** The following module defines infix operators for cleaner
    code. *)
module Infix : sig
  (** [f <$> p] is [fmap f p]. *)
  val (<$>) : ('a -> 'b) -> 'a t -> 'b t

  (** [(<*>)] is the applicative operator, the type acts
      as documentation.
      This is often used with [(<$>)] and multi-parameter
      functions, for exemple, you can write
      [(+) <$> p1 <*> p2] to make a promise containing the sum
      of the values of [p1] and [p2]. *)
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

  (** Classic bind infix operator. *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  (** Classic bind infix operator flipped. *)
  val (=<<) : ('a -> 'b t) -> 'a t -> 'b t
end
