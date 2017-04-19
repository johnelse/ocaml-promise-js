class type ['a, 'b] promise = object
  method catch : ('b -> 'a) -> ('a, 'b) promise Js.t Js.meth

  method then_1 : ('a -> 'c) -> ('c, 'b) promise Js.t Js.meth
  method then_2 : ('a -> 'c) -> ('b -> 'c) -> ('c, 'b) promise Js.t Js.meth
  method then_final : ('a -> unit) -> ('b -> unit) -> unit Js.meth
end

type 'a resolve = 'a -> unit

type 'a reject = 'a -> unit

let promise_global = Js.Unsafe.global##._Promise

let promise = promise_global

let is_supported () = Js.Optdef.test promise

let all promises =
  Js.Unsafe.fun_call promise_global##.all [|Js.Unsafe.inject promises|]

let race promises =
  Js.Unsafe.fun_call promise_global##.race [|Js.Unsafe.inject promises|]

let resolve value =
  Js.Unsafe.fun_call promise_global##.resolve [|Js.Unsafe.inject value|]

let reject value =
  Js.Unsafe.fun_call promise_global##.reject [|Js.Unsafe.inject value|]
