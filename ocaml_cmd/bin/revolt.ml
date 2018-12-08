(* open Cmdliner

open Depyt

type t = Foo | Bar of string option

let t =
    variant "v" (fun foo bar -> function Foo -> foo | Bar x -> bar x)
    |~ case0 "Foo" Foo
    |~ case1 "Bar" (option string) (fun x -> Bar x)
    |> sealv
;

Fmt.pr "t = %a\n" (dump t) Foo

compare t Foo (Bar (Some "a"))

*)
(*let revolt () = print_endline "Revolt!"*)

(*let revolt_t = Term.(Term.const(revolt) $ Term.const(()))*)

(*let () = Term.exit @@ Term.eval (revolt_t, Term.info "revolt")*)

