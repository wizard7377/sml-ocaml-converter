include File1
include File2
include File3
include File4
include File5
include File6
include File7
include File8
include File9
include File10
include File11
include File12
include File13
include File14
include File15
include File16
include File17
include File18
include File19
include File20
include File21
include File22
include File23
include File24
include Alcotest

let run_file_tests () = Alcotest.run "File_Tests" [
    "Functor_Index", [File1.TestCase.run_test];
    "Structure_Functions", [File2.TestCase.run_test];
    "ModeSyn_Datatypes", [File3.TestCase.run_test];
    "Data_Refs", [File4.TestCase.run_test];
    "TimeLimit_Exception", [File5.TestCase.run_test];
    "ArraySlice_Types", [File6.TestCase.run_test];
    "SigINT_Let", [File7.TestCase.run_test];
    "Domains_Functors", [File8.TestCase.run_test];
    "Index_Functors", [File9.TestCase.run_test];
    "TabledSyn_Functor", [File10.TestCase.run_test];
    "Flit_Large_Functor", [File11.TestCase.run_test];
    "Origins_Functor", [File12.TestCase.run_test];
    "ModSyn_Complex", [File13.TestCase.run_test];
    "Weaken_Local", [File14.TestCase.run_test];
    "Compat_Functor", [File15.TestCase.run_test];
    "Order_Functor", [File16.TestCase.run_test];
    "StyleCheck_Functor", [File17.TestCase.run_test];
    "Server_Patterns", [File18.TestCase.run_test];
    "Fixity_Datatypes", [File19.TestCase.run_test];
    "Qid_Strings", [File20.TestCase.run_test];
    "Split_Local_Refs", [File21.TestCase.run_test];
    "EVar_Nested_Let", [File22.TestCase.run_test];
    "Limit_Options", [File23.TestCase.run_test];
    "Comment_Preservation", [File24.TestCase.run_test]
]

let () = run_file_tests ()
