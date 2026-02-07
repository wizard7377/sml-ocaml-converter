open Alcotest

let test_lowercase_constructor () =
  check string "lowercase a -> A_" "A_"
    (Backend.Backend_utils.transform_constructor "a");
  check string "lowercase some -> Some_" "Some_"
    (Backend.Backend_utils.transform_constructor "some")

let test_uppercase_with_underscore () =
  check string "B_ -> B__" "B__"
    (Backend.Backend_utils.transform_constructor "B_")

let test_already_valid () =
  check string "Foo unchanged" "Foo"
    (Backend.Backend_utils.transform_constructor "Foo");
  check string "SOME -> Some (capitalize)" "Some"
    (Backend.Backend_utils.transform_constructor "SOME")

let test_variable_lowercase () =
  check string "SOME -> some_" "some_"
    (Backend.Backend_utils.transform_to_lowercase "SOME");
  check string "Foo -> foo_" "foo_"
    (Backend.Backend_utils.transform_to_lowercase "Foo");
  check string "bar -> bar" "bar"
    (Backend.Backend_utils.transform_to_lowercase "bar")

let () =
  run "Constructor_transform"
    [
      ( "constructor",
        [
          test_case "lowercase constructor" `Quick test_lowercase_constructor;
          test_case "uppercase with underscore" `Quick
            test_uppercase_with_underscore;
          test_case "already valid" `Quick test_already_valid;
        ] );
      ( "variable",
        [ test_case "variable lowercase" `Quick test_variable_lowercase ] );
    ]
