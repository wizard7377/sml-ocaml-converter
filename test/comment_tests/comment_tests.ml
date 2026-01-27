let run_file_tests () =
  Alcotest.run "File_Tests"
    [ ("Comment_1", [ Comment_test_1.TestCase.run_test ]) ]

let () = run_file_tests ()
