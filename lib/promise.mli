type ('a, 'b) promise
(** The type of promises, parameterised over a result type and a failure reason
    type. *)

type 'a resolve = 'a -> unit
(** The type of the resolve function will be passed to a promise executor
    function. *)

type 'a reject = 'a -> unit
(** The type of the reject function will be passed to a promise executor
    function. *)

val is_supported : unit -> bool
(** Test whether the Promise API is available. *)

val make : ('a resolve -> 'b reject -> unit) -> ('a, 'b) promise
(** Create a new promise given an executor function. The executor function
    will be passed a resolve and reject function, which it can use to resolve or
    reject the promise. *)

val resolve : 'a -> ('a, 'b) promise
(** Return a promise that fulfils with the given value. *)

val reject : 'b -> ('a, 'b) promise
(** Return a promise that rejects with the given value. *)

val then_bind :
  on_fulfilled:('a -> ('c ,'b) promise) ->
  ?on_rejected:('b -> ('c, 'b) promise) ->
  ('a, 'b) promise ->
  ('c, 'b) promise
(** Bind-style binding to Promise.then, which takes functions which return
    promises. *)

val then_map :
  on_fulfilled:('a -> 'c) ->
  ?on_rejected:('b -> 'c) ->
  ('a, 'b) promise ->
  ('c, 'b) promise
(** Map-style binding to Promise.then, which takes functions which return
    unwrapped values. *)

val catch_bind :
  on_rejected:('b -> ('a, 'b) promise) ->
  ('a, 'b) promise ->
  ('a, 'b) promise
(** Bind-style binding to Promise.catch, which takes a function which returns
    a promises. *)

val catch_map :
  on_rejected:('b -> 'a) ->
  ('a, 'b) promise ->
  ('a, 'b) promise
(** Map-style binding to Promise.catch, which takes functions which return
    unwrapped values. *)

val then_final :
  on_fulfilled:('a -> unit) ->
  on_rejected:('b -> unit) ->
  ('a, 'b) promise ->
  unit
(** Binding to Promise.then which can be used to end a chain of promises. Use
    this when you don't expect to have to handle any more errors, and/or want to
    finally deal with any errors thrown by the chain of Promises. *)

val all : (('a, 'b) promise) array -> ('a array, 'b) promise
(** Convert an array of promises into a single promise. If all the supplied
    promises are fulfilled, the resulting promise will be fulfilled with the
    array of results from all the supplied promise. Otherwise, the resulting
    promise will be rejected with the result of the first promise in the array
    to be rejected. *)

val race : (('a, 'b) promise) array -> ('a, 'b) promise
(** Return the result of the first promise in the supplied array to be fulfilled
    or rejected. *)

(** Infix versions of the promise chaining functions. *)
module Infix : sig

  val (>>=) : ('a, 'b) promise -> ('a -> ('c ,'b) promise) -> ('c, 'b) promise
  (** Infix version of [then_bind]. *)
  val (>|=) : ('a, 'b) promise -> ('a -> 'c) -> ('c, 'b) promise
  (** Infix version of [then_map]. *)

  val (>>~) : ('a, 'b) promise -> ('b -> ('a, 'b) promise) -> ('a, 'b) promise
  (** Infix version of [catch_bind]. *)
  val (>|~) : ('a, 'b) promise -> ('b -> 'a) -> ('a, 'b) promise
  (** Infix version of [catch_map]. *)

  val (>||) : ('a, 'b) promise -> ('a -> unit) * ('b -> unit) -> unit
  (** Infix version of [then_final]. *)
end
