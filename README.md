# Actors-OCaml
## Compiling the project

You can compile `actors-ocaml`, run the test executable and generate the documentation simply by running:

``` sh
$ dune build
$ dune test
$ dune build @doc
$ xdg-open _build/default/_doc/_html/index.html # to read the documentation, replace xdg-open by your favorite web browser
```

## Use Actors-OCaml in your code
Your `dune` file may look like

``` dune
(executable
 (name myproject)
 (libraries actorsocaml)
 (preprocess (staged_pps actorsocamlppx)))
```

Be sure to use `staged_pps` and not `pps`. It's mandatory because of a call to the type checker.


## Write a program using actors

Let's write a simple programs to calculate the fibonacci numbers.

We first want to define an actor `fib`:

``` ocaml
open Actorsocaml

let fib = object%actor (self)
  method fib n =
    if n < 2 then n
    else begin
      let f1 = Promise.await @@ self#fib (n - 1) in
      let f2 = Promise.await @@ self#fib (n - 2) in
      f1 + f2
    end
end
```

It's just as simple as that!
Don't forget to add the `actorsocamlppx` library in your `dune` file.

You are now able to send messages to `fib` like so:

``` ocaml
let main () =
  let fib_5 = fib#!fib 5 in
  Printf.printf "fib(5) = %d\n" (Promise.await fib_5)
  
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
You can also use `get`, which is blocking (In fact, it also raises an effect, but a good scheduler should block if it catches it, you don't care if you use one of the predefined schedulers).

Of course, the value will be returned if it is available.


### Actors

Actors are OCaml object running in their own domain.
The easiest way to define an actor is to use the `actorsocamlppx` syntax extension.
It's goal is to make your life easier, cause you do not want (trust me) to write all the code yourself.
At this time, you can only use `var` and `methods` in your objects: no inheritance, no private fields.
So this is a valid actor:

``` ocaml
object%actor
  val y = 42
  val get = y
end
```

The actor is running in his own thread, it is ready to use.
You can refer `self` in the definition, see the first example.

### Send messages

The methods of the generated actor returns promises, you can call the method `get` of the actor `actor` by writing 

``` ocaml
actor#!get (* Async call, returns an int Promise.t *)
actor#.get (* Sync call, returns an int *)
actor#!!get (* Forward call, see the forward section *)
```

### Mutable fields

You can of course use mutable fields in your actors, just use the standard syntax `field <- value`.

## Forward
Let's consider the following actor (taken from `examples/exple_syracuse.ml`):

``` ocaml
open Actorsocaml

let actor = object%actor (self)
  method syracuse n =
    if n = 1 then 1
    else begin
      let next = if n mod 2 = 0 then n / 2 else 3 * n + 1 in
      Promise.await @@ self#!syracuse next
    end
end

let main _ =
  let n = 42 in
  let p = Promise.await @@ actor#!syracuse n in
  assert (1 = p)

let _ = Actor.Main.run main
```

This program generates 9 differents promises (one for each call of send), and each promise has one callback function to push the continuation back to the scheduler queue.
This is very inefficient, as we create many promises that are forgotten immediately after they are fulfilled.

We could make the following change:

``` diff
-     Promise.await @@ self#!syracuse next
+     forward @@ self#!syracuse next

```

The `forward` function, will delegate the fulfillment of the promise to the called actor.
It will actually unify two promises and stop the function (by raising an exception), so the promise *is* the promise obtained by the `send` call.
The current promise to fill becomes `Forwarded (Atomic.make p')` where `p'` it the result of `send`.

We use a `union-find` like structure for the forwarded promises, with path compression.

## Exemples
See `example/exple_{pingpong; syracuse}.ml`
