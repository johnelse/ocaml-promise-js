class type ['a, 'b] promise = object
  method catch : ('b -> 'a) -> ('a, 'b) promise Js.t Js.meth

  method then_1 : ('a -> 'c) -> ('c, 'b) promise Js.t Js.meth
  method then_2 : ('a -> 'c) -> ('b -> 'c) -> ('c, 'b) promise Js.t Js.meth
  method then_final : ('a -> unit) -> ('b -> unit) -> unit Js.meth
end

type 'a resolve = 'a -> unit

type 'a reject = 'a -> unit

val promise : (('a resolve -> 'b reject -> unit) -> ('a, 'b) promise Js.t) Js.constr

val is_supported : unit -> bool

val all :
  (('a, 'b) promise Js.t) Js.js_array Js.t ->
  ('a Js.js_array Js.t, 'b) promise Js.t

val race :
  (('a, 'b) promise Js.t) Js.js_array Js.t ->
  ('a, 'b) promise Js.t

val resolve : 'a -> ('a, 'b) promise Js.t

val reject : 'b -> ('a, 'b) promise Js.t
