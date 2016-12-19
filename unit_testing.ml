module UnitTesting =
  struct
    let run_test_func name_and_func =
      let (func_name, func) = name_and_func in
      (
        (
          if func ()
          then begin print_string "OK"; true end
          else begin print_string "NOT OK"; false end
        );
        print_string ": ";
        print_string func_name;
        print_string "\n"
      )

    let run_tests test_collection =
      print_string "========= RUNNING TESTS =========\n";
      List.map run_test_func test_collection;
      print_string "========= ENDING TESTS ==========\n"

  end
;;