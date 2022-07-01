(** Promises to be fulfilled.

    This module is used by the [Actor] module
    *)

(** {1 Types, Exceptions and Effects} *)

(** The type of promises. *)
type 'a t

(** Resolver for promise, needed to fulfill. *)
type 'a resolver = 'a t

(** Effect raised by the {!await} function. *)
type _ Effect.t += NotReady : 'a t -> 'a Effect.t

(** Effect raised by the {!get} function. *)
type _ Effect.t += Get : 'a t -> 'a Effect.t

(** Effect raised by the {!async} function. *)
type _ Effect.t += Async : 'a resolver * (unit -> 'a) -> unit Effect.t

(** Raised when multiple fulfillment occur on the same promise *)
exception Promise__Multiple_Write

(** {1 Creation and access} *)

(** [create ()] creates an empty promise, it returns the
    promise and a [fill] function *)
val create : unit -> 'a t * 'a resolver

(** [resolve r v] fill the promise assiciated with r with the value [v]. *)
val resolve : 'a resolver -> 'a -> unit

(** [fail r e] fail the promise assiciated with r. *)
val fail : 'a resolver -> exn -> unit

(** [pure v] creates a promise with the value [v]
    directly available. *)
val pure : 'a -> 'a t

(** [return v] is the same as {!pure} [v], but with
    another name. *)
val return : 'a -> 'a t

(** [never_resolve] returns a resolver alone, with no associated promise. *)
val never_resolve : unit -> 'a resolver

(** [await p] raise the Effect ({!NotReady} [p])
    if [p] is empty, and return the value if
    [p] is fulfilled. If p is a failure, raise an exn. *)
val await : 'a t -> 'a

(** [await_or_exn p] raise the Effect ({!NotReady} [p])
    if [p] is empty, and return an [('a, exn) result] otherwise. *)
val await_or_exn : 'a t -> ('a, exn) result

(** [get p] blocks until the result of [p] is available
    and returns it. *)
val get : 'a t -> 'a

(** [get_or_exn p] is the same as {!await_of_exn} [p], but it may block *)
val get_or_exn : 'a t -> ('a, exn)  result

(** [is_ready p] checks if the value of [p] is available. *)
val is_ready : 'a t -> bool

(** [add_callback p f] will add the callback [f] to the
    promise [p]. When [p] will be fulfilled with a value
    [v], the promise will call [f v].
    If the promise is already fulfilled when [add_callback]
    is called, [f v] is called directly. *)
val add_callback : 'a t -> ('a -> unit) -> unit


(** {1 Useful functions} *)

(** [fmap f p] is the classic [fmap] function, it maps the
    function [f] into the promise [p]. *)
val fmap : ('a -> 'b) -> 'a t -> 'b t

(** [join p] make a promise from a promise of promise. *)
val join : 'a t t -> 'a t

(** [bind m f] is the classic monadic bind function. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** [async f] computes [f] asynchronously, it directly
    returns a promise *)
val async : (unit -> 'a) -> 'a t

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

  (** Monad sequential operator. *)
  val (>>) : 'a t -> 'b t -> 'b t

  (** Applicative [let], acts like {!fmap}. *)
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  (** Applicative [and], monoidal product operation. *)
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  (** Monadic [let] for something like [do]-notation. *)
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  (** Monadic [and] for [let*] bindings *)
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end
