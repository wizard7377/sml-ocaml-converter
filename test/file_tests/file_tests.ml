include File1 
include File2
include Alcotest
let run_file_tests () = Alcotest.run "File_Tests" [

    "Test2_Suite", [File2.TestCase.run_test];
    "Test1_Suite", [File1.TestCase.run_test]
]



let () = run_file_tests ()