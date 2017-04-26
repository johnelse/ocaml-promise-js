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

    Promise.then_final
      (fun result -> wrapper (fun () -> assert_equal result expected_result))
      (fun error  -> wrapper (fun () -> failwith "error detected"))
      promise

  let test_reject_then_final wrapper =
    let expected_error = new%js Js.error_constr (Js.string "error") in

    let promise = Promise.make (fun _ reject -> reject expected_error) in

    Promise.then_final
      (fun result -> wrapper (fun () -> failwith "result returned"))
      (fun error  -> wrapper (fun () -> assert_equal error expected_error))
      promise

  let suite =
    "then_final" >::: [
      "test_resolve_then_final" >:~ test_resolve_then_final;
      "test_reject_then_final" >:~ test_reject_then_final;
    ]
end

module Then_bind = struct
  let test_resolve wrapper =
    let promise1 = Promise.resolve 4 in
    let promise2 =
      Promise.then_bind
        ~on_fulfilled:(fun x -> Promise.resolve (x * x)) promise1
    in

    Promise.then_final
      (fun result -> wrapper (fun () -> assert_equal result 16))
      (fun error  -> wrapper (fun () -> failwith "error detected"))
      promise2

  let test_reject wrapper =
    let promise1 = Promise.resolve 4 in
    let promise2 =
      Promise.then_bind
        ~on_fulfilled:(fun x -> Promise.reject (Js.string "error")) promise1
    in

    Promise.then_final
      (fun result -> wrapper (fun () -> failwith "error did not propagate"))
      (fun error  -> wrapper (fun () -> assert_equal error (Js.string "error")))
      promise2

   let test_resolve_chained wrapper =
    let initial_value = 4 in

    let promise1 = Promise.make (fun resolve _ -> resolve initial_value) in
    let promise2 = Promise.then_bind
      ~on_fulfilled:(fun result -> Promise.resolve (result * result))
      ~on_rejected:(fun error  -> Promise.reject error)
      promise1
    in

    Promise.then_final
      (fun result -> wrapper (fun () -> assert_equal result 16))
      (fun error  -> wrapper (fun () -> failwith "error detected"))
      promise2

  let test_reject_chained wrapper =
    let initial_error = Failure "error" in

    let promise1 = Promise.make (fun _ reject -> reject initial_error) in
    let promise2 = Promise.then_bind
      ~on_fulfilled:(fun result -> failwith "error did not propagate")
      ~on_rejected:(fun error  -> Promise.resolve (Js.string "success"))
      promise1
    in

    Promise.then_final
      (fun result -> wrapper (fun () -> assert_equal result (Js.string "success")))
      (fun error  -> wrapper (fun () -> failwith "error not handled"))
      promise2

  let suite =
    "then_bind" >::: [
      "test_resolve" >:~ test_resolve;
      "test_reject" >:~ test_reject;
      "test_resolve_chained" >:~ test_resolve_chained;
      "test_reject_chained" >:~ test_reject_chained;
    ]
end

module Then_map = struct
  let test_resolve wrapper =
    let initial_value = 4 in

    let promise1 = Promise.make (fun resolve _ -> resolve initial_value) in
    let promise2 =
      Promise.then_map
        ~on_fulfilled:(fun result -> result * result) promise1
    in

    Promise.then_final
      (fun result -> wrapper (fun () -> assert_equal result 16))
      (fun error  -> wrapper (fun () -> failwith "error detected"))
      promise2

  let test_resolve_chained wrapper =
    let initial_value = 4 in

    let promise1 = Promise.make (fun resolve _ -> resolve initial_value) in
    let promise2 = Promise.then_map
      ~on_fulfilled:(fun result -> result * result)
      ~on_rejected:(fun error  -> error)
      promise1
    in

    Promise.then_final
      (fun result -> wrapper (fun () -> assert_equal result 16))
      (fun error  -> wrapper (fun () -> failwith "error detected"))
      promise2

  let test_reject_chained wrapper =
    let initial_error = Failure "error" in

    let promise1 = Promise.make (fun _ reject -> reject initial_error) in
    let promise2 = Promise.then_map
      ~on_fulfilled:(fun result -> failwith "error did not propagate")
      ~on_rejected:(fun error  -> Js.string "success")
      promise1
    in

    Promise.then_final
      (fun result -> wrapper (fun () -> assert_equal result (Js.string "success")))
      (fun error  -> wrapper (fun () -> failwith "error not handled"))
      promise2

  let test_resolve_chained_twice wrapper =
    let initial_value = 4 in

    let promise1 = Promise.make (fun resolve _ -> resolve initial_value) in
    let promise2 =
      Promise.then_map ~on_fulfilled:(fun result -> result * result) promise1 in
    let promise3 =
      Promise.then_map ~on_fulfilled:(fun result -> result > 10) promise2 in

    Promise.then_final
      (fun result -> wrapper (fun () -> assert_true result))
      (fun error  -> wrapper (fun () -> failwith "error detected"))
      promise3

  let suite =
    "then_map " >::: [
      "test_resolve" >:~ test_resolve;
      "test_resolve_chained" >:~ test_resolve_chained;
      "test_reject_chained" >:~ test_reject_chained;
      "test_resolve_chained_twice" >:~ test_resolve_chained_twice;
    ]
end

module Catch_bind = struct
  let test_catch wrapper =
    let expected_result = 123 in

    let promise1 = Promise.make (fun _ reject -> reject (Js.string "error")) in
    let promise2 =
      Promise.catch_bind (fun _ -> Promise.resolve expected_result) promise1 in

    Promise.then_final
      (fun result -> wrapper (fun () -> assert_equal result expected_result))
      (fun error  -> wrapper (fun () -> failwith "error detected"))
      promise2

  let suite =
    "catch_bind" >::: [
      "test_catch" >:~ test_catch;
    ]
end

module Catch_map = struct
  let test_catch wrapper =
    let expected_result = 123 in

    let promise1 = Promise.make (fun _ reject -> reject (Js.string "error")) in
    let promise2 = Promise.catch_map (fun _ -> expected_result) promise1 in

    Promise.then_final
      (fun result -> wrapper (fun () -> assert_equal result expected_result))
      (fun error  -> wrapper (fun () -> failwith "error detected"))
      promise2

  let suite =
    "catch_map" >::: [
      "test_catch" >:~ test_catch;
    ]
end

module Assorted_chaining = struct
  let test_then_catch_ok wrapper =
    let initial_value = 4 in

    let promise1 = Promise.make (fun resolve _ -> resolve initial_value) in
    let promise2 = Promise.then_map
      ~on_fulfilled:(fun result -> result * result)
      promise1
    in
    let promise3 = Promise.catch_map
      ~on_rejected:(fun error -> failwith "error detected")
      promise2
    in

    Promise.then_final
      (fun result -> wrapper (fun () -> assert_equal result 16))
      (fun error  -> wrapper (fun () -> failwith "error detected"))
      promise3

  let test_then_catch_error wrapper =
    let promise1 = Promise.make (fun _ reject -> reject (Js.string "error")) in
    let promise2 = Promise.then_map
      ~on_fulfilled:(fun result -> result * result)
      promise1
    in
    let promise3 = Promise.catch_map
      ~on_rejected:(fun error -> 256)
      promise2
    in

    Promise.then_final
      (fun result -> wrapper (fun () -> assert_equal result 256))
      (fun error  -> wrapper (fun () -> failwith "error detected"))
      promise3

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
      (fun result -> wrapper (fun () -> assert_equal ~printer:string_of_int (Array.length result) (Array.length expected_result)))
      (fun error  -> wrapper (fun () -> failwith "error detected"))
      (Promise.all [|promise1; promise2; promise3|])

  let test_all_reject wrapper =
    let result1 = 1 in
    let result2 = 2 in
    let expected_error = new%js Js.error_constr (Js.string "error") in

    let promise1 = Promise.make (fun resolve _ -> resolve result1) in
    let promise2 = Promise.make (fun resolve _ -> resolve result2) in
    let promise3 = Promise.make (fun _ reject -> reject expected_error) in

    Promise.then_final
      (fun result -> wrapper (fun () -> failwith "result returned"))
      (fun error  -> wrapper (fun () -> assert_equal error expected_error))
      (Promise.all [|promise1; promise2; promise3|])

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
      (fun result -> wrapper (fun () -> assert_equal ~printer:string_of_int result expected_result))
      (fun error  -> wrapper (fun () -> failwith "error detected"))
      (Promise.race [|promise|])

  let test_race_reject_one wrapper =
    let expected_error = new%js Js.error_constr (Js.string "error") in

    let promise = Promise.make (fun _ reject -> reject expected_error) in

    Promise.then_final
      (fun result -> wrapper (fun () -> failwith "result returned"))
      (fun error  -> wrapper (fun () -> assert_equal error expected_error))
      (Promise.race [|promise|])

  let test_race_resolve_two wrapper =
    let expected_result = 1 in

    let promise1 = Promise.make (fun resolve _ -> resolve expected_result) in
    let promise2 = Promise.make (fun _ _ -> ()) in

    Promise.then_final
      (fun result -> wrapper (fun () -> assert_equal result expected_result))
      (fun error  -> wrapper (fun () -> failwith "error detected"))
      (Promise.race [|promise1; promise2|])

  let test_race_reject_two wrapper =
    let expected_error = new%js Js.error_constr (Js.string "error") in

    let promise1 = Promise.make (fun _ reject -> reject expected_error) in
    let promise2 = Promise.make (fun _ _ -> ()) in

    Promise.then_final
      (fun result -> wrapper (fun () -> failwith "result returned"))
      (fun error  -> wrapper (fun () -> assert_equal error expected_error))
      (Promise.race [|promise1; promise2|])

  let suite =
    "race" >::: [
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

    Promise.then_final
      (fun result -> wrapper (fun () -> assert_equal result expected_result))
      (fun error  -> wrapper (fun () -> failwith "error detected"))
      promise

  let suite =
    "resolve" >::: [
      "test_resolve" >:~ test_resolve;
    ]
end

module Reject = struct
  let test_reject wrapper =
    let expected_error = new%js Js.error_constr (Js.string "error") in
    let promise = Promise.reject expected_error in

    Promise.then_final
      (fun result -> wrapper (fun () -> failwith "result returned"))
      (fun error  -> wrapper (fun () -> assert_equal error expected_error))
      promise

  let suite =
    "reject" >::: [
      "test_reject" >:~ test_reject;
    ]
end

module Infix = struct
  let test_resolve_then_bind wrapper =
    let open Promise.Infix in

    Promise.resolve 4
    >>= (fun x -> Promise.resolve (x * x))
    >|| (
      (fun result -> wrapper (fun () -> assert_equal result 16)),
      (fun error  -> wrapper (fun () -> failwith "error detected"))
    )

  let test_reject_then_bind wrapper =
    let open Promise.Infix in

    Promise.resolve 4
    >>= (fun x -> Promise.reject (Js.string "error"))
    >|| (
      (fun result -> wrapper (fun () -> failwith "result returned")),
      (fun error  -> wrapper (fun () -> assert_equal error (Js.string "error")))
    )

  let test_resolve_then_map wrapper =
    let open Promise.Infix in

    Promise.resolve 4
    >|= (fun x -> x * x)
    >|| (
      (fun result -> wrapper (fun () -> assert_equal result 16)),
      (fun error  -> wrapper (fun () -> failwith "error detected"))
    )

  let test_resolve_catch_bind wrapper =
    let open Promise.Infix in

    Promise.resolve 4
    >>~ (fun error -> Promise.resolve 16)
    >|| (
      (fun result -> wrapper (fun () -> assert_equal result 4)),
      (fun error  -> wrapper (fun () -> failwith "error detected"))
    )

  let test_reject_catch_bind wrapper =
    let open Promise.Infix in

    Promise.reject (Js.string "error")
    >>~ (fun error -> Promise.resolve 16)
    >|| (
      (fun result -> wrapper (fun () -> assert_equal result 16)),
      (fun error  -> wrapper (fun () -> failwith "error detected"))
    )

  let test_resolve_catch_map wrapper =
    let open Promise.Infix in

    Promise.resolve 4
    >|~ (fun error -> 16)
    >|| (
      (fun result -> wrapper (fun () -> assert_equal result 4)),
      (fun error  -> wrapper (fun () -> failwith "error detected"))
    )

  let test_reject_catch_map wrapper =
    let open Promise.Infix in

    Promise.reject (Js.string "error")
    >|~ (fun error -> 16)
    >|| (
      (fun result -> wrapper (fun () -> assert_equal result 16)),
      (fun error  -> wrapper (fun () -> failwith "error detected"))
    )

  let suite =
    "infix" >::: [
      "test_resolve_then_bind" >:~ test_resolve_then_bind;
      "test_reject_then_bind" >:~ test_reject_then_bind;
      "test_resolve_then_map" >:~ test_resolve_then_map;
      "test_resolve_catch_bind" >:~ test_resolve_catch_bind;
      "test_reject_catch_bind" >:~ test_reject_catch_bind;
      "test_resolve_catch_map" >:~ test_resolve_catch_map;
      "test_reject_catch_map" >:~ test_reject_catch_map;
    ]
end

module Http = struct
  (* Promise-based wrapper around XmlHttpRequest. *)
  let get url =
    Promise.make
      (fun resolve reject ->
        let request = XmlHttpRequest.create () in
        request##_open (Js.string "GET") (Js.string url) Js._true;

        request##.onreadystatechange :=
          Js.wrap_callback (fun () ->
            if request##.readyState = XmlHttpRequest.DONE
            then begin
              if request##.status == 200
              then resolve (request##.responseText)
              else reject (new%js Js.error_constr request##.statusText)
            end);

        request##send Js.null)

  let test_get_one_url wrapper =
    let open Promise.Infix in

    get "data/file1"
      >|| (
        (fun result ->
          wrapper (fun () -> assert_equal result (Js.string "file1"))),
        (fun error  -> wrapper (fun () -> failwith "error detected"))
      )

  let test_get_three_urls wrapper =
    let open Promise.Infix in

    Promise.all [|
      get "data/file1";
      get "data/file2";
      get "data/file3";
    |]
    >|| (
      (fun result -> wrapper (fun () ->
        assert_equal result
          [|Js.string "file1"; Js.string "file2"; Js.string "file3";|]
      )),
      (fun error  -> wrapper (fun () -> failwith "error detected"))
    )

  let suite =
    "http" >::: [
      "test_get_one_url" >:~ test_get_one_url;
      "test_get_three_urls" >:~ test_get_three_urls;
    ]
end

let suite =
  "base_suite" >::: [
    Environment.suite;
    Then_final.suite;
    Then_map.suite;
    Then_bind.suite;
    Catch_bind.suite;
    Catch_map.suite;
    Assorted_chaining.suite;
    All.suite;
    Race.suite;
    Resolve.suite;
    Reject.suite;
    Infix.suite;
    Http.suite;
  ]

let () = Webtest_js.Runner.setup suite
