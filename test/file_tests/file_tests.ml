include File1 
include File2
include Alcotest
let run_file_tests () = Alcotest.run "File Tests" [

    "Test2 Suite", [File2.TestCase.run_test];
    "Test1 Suite", [File1.TestCase.run_test]
]



let () = run_file_tests ()