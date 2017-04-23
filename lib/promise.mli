type ('a, 'b) promise

type 'a resolve = 'a -> unit

type 'a reject = 'a -> unit

val is_supported : unit -> bool

val make : ('a resolve -> 'b reject -> unit) -> ('a, 'b) promise

val resolve : 'a -> ('a, 'b) promise

val reject : 'b -> ('a, 'b) promise

val then_1_bind :
  ('a, 'b) promise -> ('a -> ('c ,'b) promise) -> ('c, 'b) promise

val then_1_map : ('a, 'b) promise -> ('a -> 'c) -> ('c, 'b) promise

val then_2_bind :
  ('a, 'b) promise ->
  ('a -> ('c, 'b) promise) ->
  ('b -> ('c, 'b) promise) ->
  ('c, 'b) promise

val then_2_map : ('a, 'b) promise -> ('a -> 'c) -> ('b -> 'c)-> ('c, 'b) promise

val catch_bind : ('a, 'b) promise -> ('b -> ('a, 'b) promise) -> ('a, 'b) promise

val catch_map : ('a, 'b) promise -> ('b -> 'a) -> ('a, 'b) promise

val then_final : ('a, 'b) promise -> ('a -> unit) -> ('b -> unit)-> unit

val all : (('a, 'b) promise) array -> ('a array, 'b) promise

val race : (('a, 'b) promise) array -> ('a, 'b) promise

module Infix : sig
  val (>>=) : ('a, 'b) promise -> ('a -> ('c ,'b) promise) -> ('c, 'b) promise
  val (>|=) : ('a, 'b) promise -> ('a -> 'c) -> ('c, 'b) promise

  val (>>~) : ('a, 'b) promise -> ('b -> ('a, 'b) promise) -> ('a, 'b) promise
  val (>|~) : ('a, 'b) promise -> ('b -> 'a) -> ('a, 'b) promise

  val (>||) : ('a, 'b) promise -> ('a -> unit) * ('b -> unit) -> unit
end
