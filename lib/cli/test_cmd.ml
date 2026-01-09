let test_config : Common.config =
  {
    input_file = "test_input.sml";
    output_file = None;
    verbosity = Some 3;
    conversions = {
      convert_names = Common.Dont_convert ;
    };
  } 