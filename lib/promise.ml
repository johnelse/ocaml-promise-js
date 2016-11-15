type 'a resolve = 'a Js.t -> unit

type 'a reject = 'a Js.t -> unit

class type ['a, 'b] promise = object
  method then_ : ('a Js.t -> unit) -> ('b Js.t -> unit) -> unit Js.meth
end

let promise = Js.Unsafe.global##._Promise

let is_supported () = Js.Optdef.test promise
