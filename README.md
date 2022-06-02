
# Table of Contents

1.  [Compiling the project](#org91e927f)
2.  [Write a program using actors](#org16f455f)
3.  [Explanations](#org5ea6c88)
    1.  [Promises](#org0332914)
        1.  [Type](#orgdd45796)
        2.  [Creating and writing](#org3d6acd8)
        3.  [Reading](#org4d91790)
    2.  [Actors](#org026ed81)
        1.  [Type](#org413f65c)
        2.  [Creation](#orge3dd9df)
        3.  [Execution](#org6e87056)
4.  [Exemples](#org426aeb1)
    1.  [Memoized Fibonacci](#org616a0fd)



<a id="org91e927f"></a>

# Compiling the project

You can compile `actors-ocaml` and run the test executable simply by running:

    $ dune build
    $ dune test


<a id="org16f455f"></a>

# Write a program using actors

Let&rsquo;s write a simple programs to calculate the fibonacci numbers.

First, we want a method acting like a function `fib : int -> int`. To do this, we create the following module:

    module MyMessage = struct
      type 'a t =
        |  Fib : int -> int t
      type method_type = { m : 'a . 'a t -> 'a }
    end
    
    module MyActor = Actor.Make(MyMessage)

The type &rsquo;a t is the type of our message. It represents our different functions. The return type is `int`, because the type of `Fib n` is `int`.
We could have define the `flip` function by adding the constructor:

    | Flip : ('a -> 'b -> 'c) * 'b * 'a -> 'c t

Once declared, we can define the `fib` function:

    let methods : type a . 'm MyActor.t -> a MyMessage.t -> a = fun self -> function
      | MyMessage.Fib n ->
        if n < 2 then n else begin
          let p1 = MyActor.send self (Fib (n - 1)) in
          let p2 = MyActor.send self (Fib (n - 2)) in
          Promise.get p1 + Promise.get p2
        end
    
    let actor_methods self = {
      MyMessage.m = fun s -> methods self s
    }

The type annotation is significant, because `MyMessage.t` is a GADT, and we want a function of type `'a . 'a MyMessage.t -> 'a`

Our actor does not need any memory, so we define a trivial `init` function:

    let init () = ()

And we can finally create the actor with `MyActor.create` (we use () because we do not need any memory):

    let actor = MyActor.create init actor_methods

The main function could look like:

    let _ =
      MyActor.run actor;
      let n = 6 in
      let p = MyActor.send actor (Fib n) in
      Printf.printf "fib(%d) = %d\n" n @@ Promise.wait_and_get p


<a id="org5ea6c88"></a>

# Explanations


<a id="org0332914"></a>

## Promises


<a id="orgdd45796"></a>

### Type

Promise are boxes where you can write only once, so something like

    type 'a promise = 'a option ref

For more readability, we define a new type (not exported anyway):

    type 'a status = Empty | Filled of 'a
    type 'a t = 'a status ref

This type is fine for simple programs, but waiting processes have no way to know when the promise is filled.
So we add a list of hooks to the empty future. These hooks will be executed when the promise it filled.
Thanks to this, the scheduler can add a hook to a promise to put a waiting process back in the queue (Faster than pushing the process to the queue directly and reading the empty promise again).

The real `status` type is:

    type 'a status =
      | Empty of ('a -> unit) list
      | Filled of 'a

As you can see, the hook takes a function `a -> unit`, so it will receive the value of the future in argument.
This is useful to define functions such as `fmap` and `join` (See `Promise.Infix`).


<a id="org3d6acd8"></a>

### Creating and writing

You can create a promise with the `create` function, and fill it with `fill`.

Trying to fill in the same promise twice raises a `Promise__Multiple_Write` error.


<a id="org4d91790"></a>

### Reading

You have two ways to read the value from a promise:

-   `wait_and_get` is blocking, so you can use it if someone else is doing the computation in *parallel*
-   `get` will throw the effect `NotReady p` if the value is not available. It can then be handled by a scheduler to compute the value *concurently*.

In both case, if the value will be returned if it is available


<a id="org026ed81"></a>

## Actors


<a id="org413f65c"></a>

### Type

An actor is a  bunch of functions, a shared memory and a scheduler, its definition is:

    type 'm t = {
      (* Currently running processes *)
      processes : process Queue.t;
      (* Memory shared between methods *)
      memory : 'm Domain.DLS.key;
      (* Methods *)
      methods : 'm t -> S.method_type
    }

The `process` type is a simple `unit -> unit`, we use a domain local state `DLS` to make the memory &ldquo;Domain local&rdquo;. S is the module containing the method type.

It is parameterized on `m`, which is the type of the shared memory.


<a id="orge3dd9df"></a>

### Creation

To create an actor, you only need to specify its methods and its shared memory.
A method is a function which takes an actor (`self`) and a message.

Do not use `Promise.wait_and_get` on a promise obtained by self, you&rsquo;ll get stuck on a value that will never be calculated.
Maybe this will be ensured by the type system in the future.


<a id="org6e87056"></a>

### Execution

To run an actor, just call the `run` function on it.
It will spawn a new thread and run the scheduler.


<a id="org426aeb1"></a>

# Exemples


<a id="org616a0fd"></a>

## Memoized Fibonacci

    open Actorsocaml
    open Promise.Infix
    
    module MyMessage = struct
      type 'a t =
        | Fib : int -> int Promise.t t
      type method_type = { m : 'a . 'a t -> 'a }
    end
    
    type memory = int Promise.t Option.t Array.t
    let init () = Array.make 20000 None
    
    module MyActor = Actor.Make(MyMessage)
    
    let methods : type a . memory MyActor.t -> a MyMessage.t -> a = fun self -> function
      | MyMessage.Fib n ->
        let m = MyActor.get_memory self in
        if m.(n) <> None then
          Option.get m.(n)
        else if n < 2 then Promise.pure n else begin
          let p1 = Promise.join @@ MyActor.send self (Fib (n - 1)) in
          let p2 = Promise.join @@ MyActor.send self (Fib (n - 2)) in
          let pres = (+) <$> p1 <*> p2 in
          m.(n) <- Some pres;
          pres
        end
    
    
    let actor_methods self = { MyMessage.m = fun s -> methods self s }
    
    let actor = MyActor.create init actor_methods
    
    let _ =
      MyActor.run actor;
      let n = 42 in
      let p = Promise.join @@ MyActor.send actor (Fib n) in
      Printf.printf "fib(%d) = %d" n @@ Promise.wait_and_get p;

