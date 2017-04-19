open Webtest.Suite

let test_is_supported () = assert_true (Promise.is_supported ())

let test_constructor () =
  let open Promise in
  let (_ : ('a, 'b) promise Js.t) = new%js promise (fun _ _ -> ()) in
  ()

let environment =
  "environment" >::: [
    "test_is_supported" >:: test_is_supported;
    "test_constructor" >:: test_constructor;
  ]

let test_resolve_then_final wrapper =
  let expected_result = Js.string "ok" in

  let promise =
    new%js Promise.promise (fun resolve _ -> resolve expected_result) in

  promise##then_final
    (fun result -> wrapper (fun () -> assert_equal result expected_result))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let test_reject_then_final wrapper =
  let expected_error = new%js Js.error_constr (Js.string "error") in

  let promise =
    new%js Promise.promise (fun _ reject -> reject expected_error) in

  promise##then_final
    (fun result -> wrapper (fun () -> failwith "result returned"))
    (fun error  -> wrapper (fun () -> assert_equal error expected_error))

let then_final =
  "then_final" >::: [
    "test_resolve_then_final" >:~ test_resolve_then_final;
    "test_reject_then_final" >:~ test_reject_then_final;
  ]

let test_resolve_chained wrapper =
  let initial_value = 4 in

  let promise1 =
    new%js Promise.promise (fun resolve _ -> resolve initial_value) in

  let promise2 = promise1##then_1 (fun result -> result * result) in

  promise2##then_final
    (fun result -> wrapper (fun () -> assert_equal result 16))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let test_resolve_chained_twice wrapper =
  let initial_value = 4 in

  let promise1 =
    new%js Promise.promise (fun resolve _ -> resolve initial_value) in

  let promise2 = promise1##then_1 (fun result -> result * result) in

  let promise3 = promise2##then_1 (fun result -> result * result) in

  promise3##then_final
    (fun result -> wrapper (fun () -> assert_equal result 256))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let then_1 =
  "then_1" >::: [
    "test_resolve_chained" >:~ test_resolve_chained;
    "test_resolve_chained_twice" >:~ test_resolve_chained_twice;
  ]

let test_resolve_chained wrapper =
  let initial_value = 4 in

  let promise1 =
    new%js Promise.promise (fun resolve _ -> resolve initial_value) in

  let promise2 = promise1##then_2
    (fun result -> result * result)
    (fun error  -> error)
  in

  promise2##then_final
    (fun result -> wrapper (fun () -> assert_equal result 16))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let test_reject_chained wrapper =
  let initial_error = Failure "error" in

  let promise1 =
    new%js Promise.promise (fun _ reject -> reject initial_error) in

  let promise2 = promise1##then_2
    (fun result -> failwith "error did not propagate")
    (fun error  -> Js.string "success")
  in

  promise2##then_final
    (fun result -> wrapper (fun () -> assert_equal result (Js.string "success")))
    (fun error  -> wrapper (fun () -> failwith "error not handled"))

let then_2 =
  "then_2" >::: [
    "test_resolve_chained" >:~ test_resolve_chained;
    "test_reject_chained" >:~ test_reject_chained;
  ]

let test_all_resolve wrapper =
  let result1 = 1 in
  let result2 = 2 in
  let result3 = 3 in
  let expected_result = Js.array [|result1; result2; result3|] in

  let promise1 =
    new%js Promise.promise (fun resolve _ -> resolve result1) in
  let promise2 =
    new%js Promise.promise (fun resolve _ -> resolve result2) in
  let promise3 =
    new%js Promise.promise (fun resolve _ -> resolve result3) in

  (Promise.all (Js.array [|promise1; promise2; promise3|]))##then_final
    (fun result -> wrapper (fun () -> assert_equal result expected_result))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let test_all_reject wrapper =
  let result1 = 1 in
  let result2 = 2 in
  let expected_error = new%js Js.error_constr (Js.string "error") in

  let promise1 =
    new%js Promise.promise (fun resolve _ -> resolve result1) in
  let promise2 =
    new%js Promise.promise (fun resolve _ -> resolve result2) in
  let promise3 =
    new%js Promise.promise (fun _ reject -> reject expected_error) in

  (Promise.all (Js.array [|promise1; promise2; promise3|]))##then_final
    (fun result -> wrapper (fun () -> failwith "result returned"))
    (fun error  -> wrapper (fun () -> assert_equal error expected_error))

let all =
  "all" >::: [
    "test_all_resolve" >:~ test_all_resolve;
    "test_all_reject" >:~ test_all_reject;
  ]

let test_race_resolve_one wrapper =
  let expected_result = 1 in

  let promise =
    new%js Promise.promise (fun resolve _ -> resolve expected_result) in

  (Promise.race (Js.array [|promise|]))##then_final
    (fun result -> wrapper (fun () -> assert_equal result expected_result))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let test_race_reject_one wrapper =
  let expected_error = new%js Js.error_constr (Js.string "error") in

  let promise =
    new%js Promise.promise (fun _ reject -> reject expected_error) in

  (Promise.race (Js.array [|promise|]))##then_final
    (fun result -> wrapper (fun () -> failwith "result returned"))
    (fun error  -> wrapper (fun () -> assert_equal error expected_error))

let test_race_resolve_two wrapper =
  let expected_result = 1 in

  let promise1 =
    new%js Promise.promise (fun resolve _ -> resolve expected_result) in
  let promise2 =
    new%js Promise.promise (fun _ _ -> ()) in

  (Promise.race (Js.array [|promise1; promise2|]))##then_final
    (fun result -> wrapper (fun () -> assert_equal result expected_result))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let test_race_reject_two wrapper =
  let expected_error = new%js Js.error_constr (Js.string "error") in

  let promise1 =
    new%js Promise.promise (fun _ reject -> reject expected_error) in
  let promise2 =
    new%js Promise.promise (fun _ _ -> ()) in

  (Promise.race (Js.array [|promise1; promise2|]))##then_final
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

  promise##then_final
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

  promise##then_final
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
    then_1;
    then_2;
    all;
    race;
    resolve;
    reject;
  ]
