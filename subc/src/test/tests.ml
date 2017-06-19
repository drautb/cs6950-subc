open OUnit2

let () =
  run_test_tt_main Parser_test.parser_tests;
  run_test_tt_main Ast_test.ast_tests;
  run_test_tt_main Typechecker_test.typechecker_tests;
  run_test_tt_main Generator_test.generator_tests;
;;
