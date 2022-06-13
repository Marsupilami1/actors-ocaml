open Actorsocaml


module MyMessage = struct
  type 'a t = Pass : unit -> unit t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end

module MyActor = Actor.Make(Roundrobin)(MyMessage)


let actor_methods =
  let methods
    : type a . MyActor.t
      -> (a Promise.t -> a)
      -> a MyMessage.t
      -> a =
    fun _ _ -> function
      | Pass () -> ()
  in
  fun self -> {MyMessage.m = fun forward -> methods self forward}


let main _ =
  print_endline "-----TEST BENCH-----";

  let actor = MyActor.create actor_methods in
  let message = MyMessage.Pass () in
  ignore @@ MyActor.send actor message;
  Unix.sleep 1;
  print_endline "Test passed";
  print_endline "--------------------"

let _ = Actor.Main.run main
