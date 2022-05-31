type 'a t
val make_empty : unit -> 'a t
val fork : (unit -> 'a) * 'a t -> unit
val get : 'a t -> ('a, exn) result
val get_val : 'a t -> 'a
val pure : 'a -> 'a t
val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val run : (unit -> 'a) -> ('a, exn) result
