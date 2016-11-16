type 'a resolve = 'a -> unit

type 'a reject = 'a -> unit

class type ['a, 'b] promise = object
  method then_ : ('a -> unit) -> ('b -> unit) -> unit Js.meth
end

let promise_global = Js.Unsafe.global##._Promise

let promise = promise_global

let is_supported () = Js.Optdef.test promise

let all promises =
  Js.Unsafe.fun_call promise_global##.all [|Js.Unsafe.inject promises|]
