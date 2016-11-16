type 'a resolve = 'a -> unit

type 'a reject = 'a -> unit

class type ['a, 'b] promise = object
  method then_ : ('a -> unit) -> ('b -> unit) -> unit Js.meth
end

let promise = Js.Unsafe.global##._Promise

let is_supported () = Js.Optdef.test promise
