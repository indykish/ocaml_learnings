

let revolt () = match Array.to_list Sys.argv with
| [_; in_file] ->
    begin try
      let pdf = Pdfread.pdf_of_file None None in_file in
          Pdfwrite.debug_whole_pdf pdf;
          print_endline "Done"
    with
      err ->
        Printf.printf "Failed to decompress file.\n%s\n\n" (Printexc.to_string err);
    end
|_ -> print_endline "nah none matches"

(* 
 * let revolt_t = Term.(Term.const(revolt) $ Term.const(()))
 *
 * let () = Term.exit @@ Term.eval (revolt_t, Term.info "pdfscrap") 
*)

let () = revolt()

