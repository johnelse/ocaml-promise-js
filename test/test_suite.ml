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

  (Promise.all (Js.array [|promise1; promise2; promise3|]))##then_
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

  (Promise.all (Js.array [|promise1; promise2; promise3|]))##then_
    (fun result -> wrapper (fun () -> failwith "result returned"))
    (fun error  -> wrapper (fun () -> assert_equal error expected_error))

let all =
  "all" >::: [
    "test_all_resolve" >:~ test_all_resolve;
    "test_all_reject" >:~ test_all_reject;
  ]

let suite =
  "base_suite" >::: [
    environment;
    then_;
    all;
  ]
