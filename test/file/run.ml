include File1 
include File2
let () = Alcotest.run "File Tests" [
    "Test1 Suite", [File1.TestCase.run_test];
    "Test2 Suite", [File2.TestCase.run_test]
]