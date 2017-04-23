type ('a, 'b) promise

type 'a resolve = 'a -> unit

type 'a reject = 'a -> unit

let promise_global = Js.Unsafe.global##._Promise

let is_supported () = Js.Optdef.test promise_global

let make f =
  Js.Unsafe.new_obj promise_global [|Js.Unsafe.inject f|]

let resolve value =
  Js.Unsafe.fun_call promise_global##.resolve [|Js.Unsafe.inject value|]

let reject value =
  Js.Unsafe.fun_call promise_global##.reject [|Js.Unsafe.inject value|]

let catch promise f_error =
  Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject f_error|]

let then_1 promise f_ok =
  Js.Unsafe.meth_call promise "then" [|Js.Unsafe.inject f_ok|]

let then_2 promise f_ok f_error =
  Js.Unsafe.meth_call
    promise "then"
    [|Js.Unsafe.inject f_ok; Js.Unsafe.inject f_error|]

let then_final promise f_ok f_error =
  Js.Unsafe.meth_call
    promise "then"
    [|Js.Unsafe.inject f_ok; Js.Unsafe.inject f_error|]

let all promises =
  let intermediate_promise =
    Js.Unsafe.fun_call promise_global##.all
      [|Js.Unsafe.inject (Js.array promises)|]
  in
  then_1 intermediate_promise (fun js_array -> Js.to_array js_array)

let race promises =
  Js.Unsafe.fun_call promise_global##.race
    [|Js.Unsafe.inject (Js.array promises)|]
