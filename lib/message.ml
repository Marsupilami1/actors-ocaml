module type S = sig
  type 'a t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end
