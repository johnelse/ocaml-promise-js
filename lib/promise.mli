type 'a resolve = 'a Js.t -> unit

type 'a reject = 'a Js.t -> unit

class type ['a, 'b] promise = object
  method then_ : ('a Js.t -> unit) -> ('b Js.t -> unit) -> unit Js.meth
end

val promise : (('a resolve -> 'b reject -> unit) -> ('a, 'b) promise Js.t) Js.constr

val is_supported : unit -> bool
