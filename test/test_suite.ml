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

let test_then_resolve wrapper =
  let expected_result = Js.string "ok" in

  let promise =
    new%js Promise.promise (fun resolve _ -> resolve expected_result) in

  promise##then_
    (fun result -> wrapper (fun () -> assert_equal result expected_result))
    (fun error  -> wrapper (fun () -> failwith "error detected"))

let test_then_reject wrapper =
  let expected_error = new%js Js.error_constr (Js.string "error") in

  let promise =
    new%js Promise.promise (fun _ reject -> reject expected_error) in

  promise##then_
    (fun result -> wrapper (fun () -> failwith "result returned"))
    (fun error  -> wrapper (fun () -> assert_equal error expected_error))

let then_ =
  "then" >::: [
    "test_then_resolve" >:~ test_then_resolve;
    "test_then_reject" >:~ test_then_reject;
  ]

let suite =
  "base_suite" >::: [
    environment;
    then_;
  ]
