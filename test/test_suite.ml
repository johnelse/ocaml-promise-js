open Webtest.Suite

module Environment = struct
  let test_is_supported () = assert_true (Promise.is_supported ())

  let test_constructor () =
    let open Promise in
    let (_ : ('a, 'b) promise) = Promise.make (fun _ _ -> ()) in
    ()

  let suite =
    "environment" >::: [
      "test_is_supported" >:: test_is_supported;
      "test_constructor" >:: test_constructor;
    ]
end

module Then_final = struct
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

  let suite =
    "then_final" >::: [
      "test_resolve_then_final" >:~ test_resolve_then_final;
      "test_reject_then_final" >:~ test_reject_then_final;
    ]
end

module Catch_map = struct
  let test_catch wrapper =
    let expected_result = 123 in

    let promise1 = Promise.make (fun _ reject -> reject (Js.string "error")) in

    let promise2 = Promise.catch_map promise1 (fun _ -> expected_result) in

    Promise.then_final promise2
      (fun result -> wrapper (fun () -> assert_equal result expected_result))
      (fun error  -> wrapper (fun () -> failwith "error detected"))

  let suite =
    "catch_map" >::: [
      "test_catch" >:~ test_catch;
    ]
end

module Then_1_bind = struct
  let test_resolve wrapper =
    let promise1 = Promise.resolve 4 in
    let promise2 =
      Promise.then_1_bind promise1 (fun x -> Promise.resolve (x * x)) in

    Promise.then_final promise2
      (fun result -> wrapper (fun () -> assert_equal result 16))
      (fun error  -> wrapper (fun () -> failwith "error detected"))

  let test_reject wrapper =
    let promise1 = Promise.resolve 4 in
    let promise2 =
      Promise.then_1_bind promise1 (fun x -> Promise.reject (Js.string "error"))
    in

    Promise.then_final promise2
      (fun result -> wrapper (fun () -> failwith "error did not propagate"))
      (fun error  -> wrapper (fun () -> assert_equal error (Js.string "error")))

  let suite =
    "then_1_bind" >::: [
      "test_resolve" >:~ test_resolve;
      "test_reject" >:~ test_reject;
    ]
end

module Then_1_map = struct
  let test_resolve_chained wrapper =
    let initial_value = 4 in

    let promise1 = Promise.make (fun resolve _ -> resolve initial_value) in

    let promise2 = Promise.then_1_map promise1 (fun result -> result * result) in

    Promise.then_final promise2
      (fun result -> wrapper (fun () -> assert_equal result 16))
      (fun error  -> wrapper (fun () -> failwith "error detected"))

  let test_resolve_chained_twice wrapper =
    let initial_value = 4 in

    let promise1 = Promise.make (fun resolve _ -> resolve initial_value) in

    let promise2 = Promise.then_1_map promise1 (fun result -> result * result) in

    let promise3 = Promise.then_1_map promise2 (fun result -> result > 10) in

    Promise.then_final promise3
      (fun result -> wrapper (fun () -> assert_true result))
      (fun error  -> wrapper (fun () -> failwith "error detected"))

  let suite =
    "then_1_map " >::: [
      "test_resolve_chained" >:~ test_resolve_chained;
      "test_resolve_chained_twice" >:~ test_resolve_chained_twice;
    ]
end

module Then_2_bind = struct
  let test_resolve_chained wrapper =
    let initial_value = 4 in

    let promise1 = Promise.make (fun resolve _ -> resolve initial_value) in

    let promise2 = Promise.then_2_bind promise1
      (fun result -> Promise.resolve (result * result))
      (fun error  -> Promise.reject error)
    in

    Promise.then_final promise2
      (fun result -> wrapper (fun () -> assert_equal result 16))
      (fun error  -> wrapper (fun () -> failwith "error detected"))

  let test_reject_chained wrapper =
    let initial_error = Failure "error" in

    let promise1 = Promise.make (fun _ reject -> reject initial_error) in

    let promise2 = Promise.then_2_bind promise1
      (fun result -> failwith "error did not propagate")
      (fun error  -> Promise.resolve (Js.string "success"))
    in

    Promise.then_final promise2
      (fun result -> wrapper (fun () -> assert_equal result (Js.string "success")))
      (fun error  -> wrapper (fun () -> failwith "error not handled"))

  let suite =
    "then_2_bind" >::: [
      "test_resolve_chained" >:~ test_resolve_chained;
      "test_reject_chained" >:~ test_reject_chained;
    ]
end

module Then_2_map = struct
  let test_resolve_chained wrapper =
    let initial_value = 4 in

    let promise1 = Promise.make (fun resolve _ -> resolve initial_value) in

    let promise2 = Promise.then_2_map promise1
      (fun result -> result * result)
      (fun error  -> error)
    in

    Promise.then_final promise2
      (fun result -> wrapper (fun () -> assert_equal result 16))
      (fun error  -> wrapper (fun () -> failwith "error detected"))

  let test_reject_chained wrapper =
    let initial_error = Failure "error" in

    let promise1 = Promise.make (fun _ reject -> reject initial_error) in

    let promise2 = Promise.then_2_map promise1
      (fun result -> failwith "error did not propagate")
      (fun error  -> Js.string "success")
    in

    Promise.then_final promise2
      (fun result -> wrapper (fun () -> assert_equal result (Js.string "success")))
      (fun error  -> wrapper (fun () -> failwith "error not handled"))

  let suite =
    "then_2_map" >::: [
      "test_resolve_chained" >:~ test_resolve_chained;
      "test_reject_chained" >:~ test_reject_chained;
    ]
end

module Assorted_chaining = struct
  let test_then_catch_ok wrapper =
    let initial_value = 4 in

    let promise1 = Promise.make (fun resolve _ -> resolve initial_value) in

    let promise2 = Promise.then_1_map promise1
      (fun result -> result * result) in

    let promise3 = Promise.catch_map promise2
      (fun error -> failwith "error detected") in

    Promise.then_final promise3
      (fun result -> wrapper (fun () -> assert_equal result 16))
      (fun error  -> wrapper (fun () -> failwith "error detected"))

  let test_then_catch_error wrapper =
    let promise1 = Promise.make (fun _ reject -> reject (Js.string "error")) in

    let promise2 = Promise.then_1_map promise1
      (fun result -> result * result) in

    let promise3 = Promise.catch_map promise2
      (fun error -> 256) in

    Promise.then_final promise3
      (fun result -> wrapper (fun () -> assert_equal result 256))
      (fun error  -> wrapper (fun () -> failwith "error detected"))

  let suite =
    "assorted_chaining" >::: [
      "test_then_catch_ok" >:~ test_then_catch_ok;
      "test_then_catch_error" >:~ test_then_catch_error;
    ]
end

module All = struct
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

  let suite =
    "all" >::: [
      "test_all_resolve" >:~ test_all_resolve;
      "test_all_reject" >:~ test_all_reject;
    ]
end

module Race = struct
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

  let suite =
    "race" >:::
      [
        "test_race_resolve_one" >:~ test_race_resolve_one;
        "test_race_reject_one" >:~ test_race_reject_one;
        "test_race_resolve_two" >:~ test_race_resolve_two;
        "test_race_reject_two" >:~ test_race_reject_two;
      ]
end

module Resolve = struct
  let test_resolve wrapper =
    let expected_result = 42 in
    let promise = Promise.resolve expected_result in

    Promise.then_final promise
      (fun result -> wrapper (fun () -> assert_equal result expected_result))
      (fun error  -> wrapper (fun () -> failwith "error detected"))

  let suite =
    "resolve" >:::
      [
        "test_resolve" >:~ test_resolve;
      ]
end

module Reject = struct
  let test_reject wrapper =
    let expected_error = new%js Js.error_constr (Js.string "error") in
    let promise = Promise.reject expected_error in

    Promise.then_final promise
      (fun result -> wrapper (fun () -> failwith "result returned"))
      (fun error  -> wrapper (fun () -> assert_equal error expected_error))

  let suite =
    "reject" >:::
      [
        "test_reject" >:~ test_reject;
      ]
end

let suite =
  "base_suite" >::: [
    Environment.suite;
    Then_final.suite;
    Then_1_map.suite;
    Then_1_bind.suite;
    Then_2_map.suite;
    Then_2_bind.suite;
    Catch_map.suite;
    Assorted_chaining.suite;
    All.suite;
    Race.suite;
    Resolve.suite;
    Reject.suite
  ]
