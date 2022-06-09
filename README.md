# Actors-OCaml
## Compiling the project

You can compile `actors-ocaml`, run the test executable and generate the documentation simply by running:

``` sh
$ dune build
$ dune test
$ dune build @doc
$ xdg-open _build/default/_doc/_html/index.html # to read the documentation, replace xdg-open by your favorite web browser
```


## Write a program using actors

Let's write a simple programs to calculate the fibonacci numbers.

First, we want a method acting like a function `fib : int -> int`. To do this, we create the following module:

``` ocaml
module MyMessage = struct
  type 'a t =
    |  Fib : int -> int t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end

(* Roundrobin is a predefined scheduler *)
module MyActor = Actor.Make(Roundrobin)(MyMessage)
```


The type `'a t` is the type of our message. It represents our different functions. The return type is `int`, because the type of `Fib n` is `int`.
We could have define the `flip` function by adding the constructor:

``` ocaml
    | Flip : ('a -> 'b -> 'c) * 'b * 'a -> 'c t
```

Once declared, we can define the `fib` function:

``` ocaml
let actor_methods =
  let methods
    : type a . memory MyActor.t (* Actor *)
      -> (a Promise.t -> a) (* forward function, not used here*)
      -> a MyMessage.t (* message received *)
      -> a (* return type *)
    = fun self _ -> function
      | Fib n ->
        if n < 2 then n else begin
          let p1 = MyActor.send self (Fib (n - 1)) in
          let p2 = MyActor.send self (Fib (n - 2)) in
          Promise.await p1 + Promise.await p2
        end
  in fun self ->
    {MyMessage.m = fun forward -> methods self forward}

The type annotation is significant, because `MyMessage.t` is a GADT, and we want a function of type `'a . 'a MyMessage.t -> 'a` (In fact, we want a `'a . ('a Promise.t -> 'a) -> 'a MyMessage.t -> 'a`, see the Forward section)
```

And we can finally create the actor with `MyActor.create`:

``` ocaml
let actor = MyActor.create actor_methods
```

The main function could look like:

``` ocaml
let main _ =
  MyActor.run actor;
  let n = 6 in
  let p = MyActor.send actor (Fib n) in
  Printf.printf "fib(%d) = %d\n" n @@ Promise.await p;
  MyActor.Stop actor
  
let _ = Actor.Main.run main
```

## Explanations
### Promises
#### Type

Promise are boxes where you can write only once, so something like

``` ocaml
type 'a promise = 'a option ref
```

For more readability, we define a new type (not exported anyway):

``` ocaml
type 'a status = Empty | Filled of 'a
type 'a t = 'a status ref
```

This type is fine for simple programs, but waiting processes have no way to know when the promise is filled.
So we add a list of callbacks to the empty future. These callbacks will be executed when the promise it fulfilled.
Thanks to this, the scheduler can add a callback to a promise to put a waiting process back in the queue (Faster than pushing the process to the queue directly and reading the empty promise again).

The real `status` type is (the `Forward` constructor is explained in the Forward section):
``` ocaml
type 'a status =
  | Empty of ('a -> unit) list
  | Filled of 'a
  | Forwarded of 'a t Atomic.t
and 'a t = 'a status Atomic.t
```

As you can see, the callback takes a function `'a -> unit`, so it will receive the value of the future in argument.
This is useful to define functions such as `fmap` and `join` (See `Promise.Infix`).

#### Creating and writing

You can create a promise with the `create` function, and fill it with `fill`.

Trying to fill in the same promise twice raises a `Promise__Multiple_Write` error.

#### Reading

You can get the value of a promise with `await`. It will throw the effect `NotReady p` if the value is not available, so it can then be handled by a scheduler (That's why the call to `Actor.Main.run` is mandatory).

Of course, the value will be returned if it is available.


### Actors
#### Type

An actor is a  bunch of functions, a shared memory and a scheduler, its definition is:

``` ocaml
type t = {
  (* Currently running processes *)
  processes : process Domainslib.Chan.t;
  (* Methods *)
  methods : t -> S.method_type;
  (* Domain in which the actor is running *)
  mutable domain : unit Domain.t Option.t
}
```

The `process` type is a simple `unit -> unit`, we use a domain local state `DLS` to make the memory "Domain local". `S` is the module containing the method type (See the exemple).

#### Creation

To create an actor, you only need to specify its methods.
A method is a function which takes an actor (`self`) and a message.

#### Execution
To run an actor, just call the `run` function on it.
It will spawn a new thread and run the scheduler.

You need to stop actors yourself for now, I hope this will be automatic in the future.

## Forward
Let's consider the following actor:

``` ocaml
open Actorsocaml

module MyMessage = struct
  type 'a t =
    | Syracuse : int -> int Promise.t t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end

type memory = unit
let init () = ()

module MyActor = Actor.Make(Roundrobin)(MyMessage)

let methods
  : type a . memory MyActor.t
    -> (a Promise.t -> a)
    -> a MyMessage.t
    -> a
  = fun self _ -> function
  | Syracuse n ->
    if n = 1 then
      Promise.pure 1
    else begin
      let next = if n mod 2 = 0 then n / 2 else 3 * n + 1 in
      Promise.await @@ MyActor.send self (Syracuse(next))
    end
let actor_methods self = {
  MyMessage.m = fun s -> methods self s
}

let main _ =
  let actor = MyActor.create init actor_methods in
  let ra = MyActor.run actor in

  let n = 42 in
  let p = Promise.join @@ MyActor.send actor (Syracuse n) in
  ignore @@ Promise.await p;

  MyActor.stop actor ra;
  
let _ = Actor.Main.run main
```

This program generates 9 differents promises (one for each call of send), and each promise has one callback function to push the continuation back to the scheduler queue.
This is very inefficient, as we create many promises that are forgotten immediately after they are fulfilled.

We could make the following change:

``` diff
-     Promise.await @@ MyActor.send self (Syracuse(next))
+     Promise.join @@ MyActor.send self (Syracuse(next))
```
This will not decrease the number of promises (it will actually be higher), but the scheduler does not wait for promises to be fulfilled. Instead, we add a callback to each promise to directly fill the next promise. So this program is faster than the previous one

But the faster solution is to use forward:

``` diff
- = fun self _ -> function
+ = fun self forward -> function
-     Promise.await @@ MyActor.send self (Syracuse(next))
+     forward @@ MyActor.send self (Syracuse(next))
```

The `forward` function provided in the function parameter will delegate the fulfillment of the promise to the called actor.
It will actually unify two promises and stop the function (by raising an exception), so the promise *is* the promise obtained by the `send` call.
The current promise to fill becomes `Forwarded (Atomic.make p')` where `p'` it the result of `send`.

We use a `union-find` like structure for the forwarded promises, with path compression.

## Exemples
See `test/test_{actor; pingpong; ring; condition; bench}.ml`
