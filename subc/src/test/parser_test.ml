open OUnit2
open Core

let load_programs path =
  let base = (Sys.getcwd ()) ^ "/parser_examples/" ^ path ^ "/" in
  let file_list = Sys.readdir base in
  List.map (List.filter (Array.to_list file_list)
              ~f:(fun filename ->
                  (String.equal ".c"
                     (String.sub filename ~pos:((String.length filename) - 2) ~len:2))))
    ~f:(fun filename ->
        let file = In_channel.create (base ^ filename) in
        let contents = In_channel.input_all file in
        let test_case = (filename, contents) in
        In_channel.close file;
        test_case)
;;

let accepted_programs = load_programs "accept"
let rejected_programs = load_programs "reject"

let parser_tests =
  "parser_tests" >:::
  (List.append
     (List.map accepted_programs
        ~f:(fun (name, prog) ->
            name >::
            (fun _ ->
               let _ = Subc.ast_of_string prog in ())))

     (List.map rejected_programs
        ~f:(fun (name, prog) ->
            name >::
            (fun _ ->
               let f = fun () -> Subc.ast_of_string prog in
               assert_raises Parser.Error f
                 ~msg:(Printf.sprintf "Expected program '%s' to not parse!" name)))))
;;
