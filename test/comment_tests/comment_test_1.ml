include Common_tests

module Test1 : TEST_FILES = struct
  let test_name = "TEST 1"
  let input_file = "file1_input.sml"

  let expected_strings =
    [
      {
        content =
          {|(* targetHeadOpt (V) = SOME(H) or NONE
     where H is the head of the atomic target type of V,
     NONE if V is a kind or object or have variable type.
     Does not expand type definitions.
  *)|};
        start_percent = 0.0;
        end_percent = 0.9;
      };
      {
        content =
          {|(* targetHeadOpt (V) = SOME(H) or NONE
     where H is the head of the atomic target type of V,
     NONE if V is a kind or object or have variable type.
     Does not expand type definitions.
  *)|};
        start_percent = 0.4;
        end_percent = 1.0;
      };
    ]
end

module TestCase = Common_tests.Make (Test1)
