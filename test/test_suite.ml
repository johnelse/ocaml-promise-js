open Webtest.Suite

let test_is_supported () = assert_true (Promise.is_supported ())

let test_constructor () =
  let open Promise in
  let (_ : ('a, 'b) promise) = Promise.make (fun _ _ -> ()) in
  ()

let environment =
  "environment" >::: [
    "test_is_supported" >:: test_is_supported;
    "test_constructor" >:: test_constructor;
  ]

let test_resolve_then_final wrapper =
  let expected_result = Js.string "ok" in

  let promise = Promise.make (fun resolve _ -> resolve expected_result) in

  Promise.then_final promise
    (fun result -> wrapper (fun () -> assert_equal result expected_result))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let test_reject_then_final wrapper =
  let expected_error = new%js Js.error_constr (Js.string "error") in

  let promise = Promise.make (fun _ reject -> reject expected_error) in

  Promise.then_final promise
    (fun result -> wrapper (fun () -> failwith "result returned"))
    (fun error  -> wrapper (fun () -> assert_equal error expected_error))

let then_final =
  "then_final" >::: [
    "test_resolve_then_final" >:~ test_resolve_then_final;
    "test_reject_then_final" >:~ test_reject_then_final;
  ]

let test_catch wrapper =
  let expected_result = 123 in

  let promise1 = Promise.make (fun _ reject -> reject (Js.string "error")) in

  let promise2 = Promise.catch promise1 (fun _ -> expected_result) in

  Promise.then_final promise2
    (fun result -> wrapper (fun () -> assert_equal result expected_result))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let catch =
  "catch" >::: [
    "test_catch" >:~ test_catch;
  ]

let test_resolve_chained wrapper =
  let initial_value = 4 in

  let promise1 = Promise.make (fun resolve _ -> resolve initial_value) in

  let promise2 = Promise.then_1 promise1 (fun result -> result * result) in

  Promise.then_final promise2
    (fun result -> wrapper (fun () -> assert_equal result 16))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let test_resolve_chained_twice wrapper =
  let initial_value = 4 in

  let promise1 = Promise.make (fun resolve _ -> resolve initial_value) in

  let promise2 = Promise.then_1 promise1 (fun result -> result * result) in

  let promise3 = Promise.then_1 promise2 (fun result -> result * result) in

  Promise.then_final promise3
    (fun result -> wrapper (fun () -> assert_equal result 256))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let then_1 =
  "then_1" >::: [
    "test_resolve_chained" >:~ test_resolve_chained;
    "test_resolve_chained_twice" >:~ test_resolve_chained_twice;
  ]

let test_resolve_chained wrapper =
  let initial_value = 4 in

  let promise1 = Promise.make (fun resolve _ -> resolve initial_value) in

  let promise2 = Promise.then_2 promise1
    (fun result -> result * result)
    (fun error  -> error)
  in

  Promise.then_final promise2
    (fun result -> wrapper (fun () -> assert_equal result 16))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let test_reject_chained wrapper =
  let initial_error = Failure "error" in

  let promise1 = Promise.make (fun _ reject -> reject initial_error) in

  let promise2 = Promise.then_2 promise1
    (fun result -> failwith "error did not propagate")
    (fun error  -> Js.string "success")
  in

  Promise.then_final promise2
    (fun result -> wrapper (fun () -> assert_equal result (Js.string "success")))
    (fun error  -> wrapper (fun () -> failwith "error not handled"))

let then_2 =
  "then_2" >::: [
    "test_resolve_chained" >:~ test_resolve_chained;
    "test_reject_chained" >:~ test_reject_chained;
  ]

let test_then_catch_ok wrapper =
  let initial_value = 4 in

  let promise1 = Promise.make (fun resolve _ -> resolve initial_value) in

  let promise2 = Promise.then_1 promise1
    (fun result -> result * result) in

  let promise3 = Promise.catch promise2
    (fun error -> failwith "error detected") in

  Promise.then_final promise3
    (fun result -> wrapper (fun () -> assert_equal result 16))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let test_then_catch_error wrapper =
  let promise1 = Promise.make (fun _ reject -> reject (Js.string "error")) in

  let promise2 = Promise.then_1 promise1
    (fun result -> result * result) in

  let promise3 = Promise.catch promise2
    (fun error -> 256) in

  Promise.then_final promise3
    (fun result -> wrapper (fun () -> assert_equal result 256))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let assorted_chaining =
  "assorted_chaining" >::: [
    "test_then_catch_ok" >:~ test_then_catch_ok;
    "test_then_catch_error" >:~ test_then_catch_error;
  ]

let test_all_resolve wrapper =
  let result1 = 1 in
  let result2 = 2 in
  let result3 = 3 in
  let expected_result = [|result1; result2; result3|] in

  let promise1 = Promise.make (fun resolve _ -> resolve result1) in
  let promise2 = Promise.make (fun resolve _ -> resolve result2) in
  let promise3 = Promise.make (fun resolve _ -> resolve result3) in

  Promise.then_final
    (Promise.all [|promise1; promise2; promise3|])
    (fun result -> wrapper (fun () -> assert_equal ~printer:string_of_int (Array.length result) (Array.length expected_result)))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let test_all_reject wrapper =
  let result1 = 1 in
  let result2 = 2 in
  let expected_error = new%js Js.error_constr (Js.string "error") in

  let promise1 = Promise.make (fun resolve _ -> resolve result1) in
  let promise2 = Promise.make (fun resolve _ -> resolve result2) in
  let promise3 = Promise.make (fun _ reject -> reject expected_error) in

  Promise.then_final
    (Promise.all [|promise1; promise2; promise3|])
    (fun result -> wrapper (fun () -> failwith "result returned"))
    (fun error  -> wrapper (fun () -> assert_equal error expected_error))

let all =
  "all" >::: [
    "test_all_resolve" >:~ test_all_resolve;
    "test_all_reject" >:~ test_all_reject;
  ]

let test_race_resolve_one wrapper =
  let expected_result = 1 in

  let promise = Promise.make (fun resolve _ -> resolve expected_result) in

  Promise.then_final
    (Promise.race [|promise|])
    (fun result -> wrapper (fun () -> assert_equal ~printer:string_of_int result expected_result))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let test_race_reject_one wrapper =
  let expected_error = new%js Js.error_constr (Js.string "error") in

  let promise = Promise.make (fun _ reject -> reject expected_error) in

  Promise.then_final
    (Promise.race [|promise|])
    (fun result -> wrapper (fun () -> failwith "result returned"))
    (fun error  -> wrapper (fun () -> assert_equal error expected_error))

let test_race_resolve_two wrapper =
  let expected_result = 1 in

  let promise1 = Promise.make (fun resolve _ -> resolve expected_result) in
  let promise2 = Promise.make (fun _ _ -> ()) in

  Promise.then_final
    (Promise.race [|promise1; promise2|])
    (fun result -> wrapper (fun () -> assert_equal result expected_result))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let test_race_reject_two wrapper =
  let expected_error = new%js Js.error_constr (Js.string "error") in

  let promise1 = Promise.make (fun _ reject -> reject expected_error) in
  let promise2 = Promise.make (fun _ _ -> ()) in

  Promise.then_final
    (Promise.race [|promise1; promise2|])
    (fun result -> wrapper (fun () -> failwith "result returned"))
    (fun error  -> wrapper (fun () -> assert_equal error expected_error))

let race =
  "race" >:::
    [
      "test_race_resolve_one" >:~ test_race_resolve_one;
      "test_race_reject_one" >:~ test_race_reject_one;
      "test_race_resolve_two" >:~ test_race_resolve_two;
      "test_race_reject_two" >:~ test_race_reject_two;
    ]

let test_resolve wrapper =
  let expected_result = 42 in
  let promise = Promise.resolve expected_result in

  Promise.then_final promise
    (fun result -> wrapper (fun () -> assert_equal result expected_result))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let resolve =
  "resolve" >:::
    [
      "test_resolve" >:~ test_resolve;
    ]

let test_reject wrapper =
  let expected_error = new%js Js.error_constr (Js.string "error") in
  let promise = Promise.reject expected_error in

  Promise.then_final promise
    (fun result -> wrapper (fun () -> failwith "result returned"))
    (fun error  -> wrapper (fun () -> assert_equal error expected_error))

let reject =
  "reject" >:::
    [
      "test_reject" >:~ test_reject;
    ]

let suite =
  "base_suite" >::: [
    environment;
    then_final;
    catch;
    then_1;
    then_2;
    assorted_chaining;
    all;
    race;
    resolve;
    reject;
  ]
