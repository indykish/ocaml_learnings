(*
let () =
  let line : string =
    Pervasives.read_line () in
  print_endline "Now unblocked!";
  ignore line
*)

(*
let () =
  let line_promise : string Lwt.t =
    Lwt_io.(read_line stdin) in
  print_endline "Execution just continues...";


  let line : string = 
    Lwt_main.run line_promise in
  print_endline line

*)
(*
let () =
  let p : unit Lwt.t =
    let%lwt line_1 = Lwt_io.(read_line stdin) in
    let%lwt line_2 = Lwt_io.(read_line stdin) in
    Lwt_io.printf "%s and %s\n" line_1 line_2
  in

  Lwt_main.run p
*)

(*

let () =
  let p : unit Lwt.t =
    let line_1_promise : string Lwt.t = Lwt_io.(read_line stdin) in
    Lwt.bind line_1_promise (fun (line_1 : string) ->

        let line_2_promise : string Lwt.t = Lwt_io.(read_line stdin) in
        Lwt.bind line_2_promise (fun (line_2 : string) ->

            Lwt_io.printf "%s and %s\n" line_1 line_2))
  in

  Lwt_main.run p

*)

(*
let () =
  Lwt_main.run begin
    let three_seconds : unit Lwt.t = Lwt_unix.sleep 3.0 in
    let%lwt () = three_seconds in
    let%lwt () = Lwt_io.printl "3 seconds passed" in
    Lwt_io.printf "Only 2 more seconds passed %s" " hey !!!"
  end
*)

(* 
let () =
  Lwt_main.run begin
    let three_seconds : unit Lwt.t = Lwt_unix.sleep 13. in
    let five_seconds : unit Lwt.t = Lwt_unix.sleep 5. in

    (* Both waits have already been started at this point! *)

    Lwt.bind three_seconds (fun () ->
        (* This is 3 seconds later. *)
        Lwt.bind (Lwt_io.printl "13 seconds passed") (fun () ->
            Lwt.bind five_seconds (fun () ->
                (* Only 2 seconds were left in the 5-second wait, so
                    this callback runs 2 seconds after the first callback. *)
                Lwt_io.printl "Only 2 more seconds passed")))
  end
*)

(*
let () =
  Lwt_main.run begin
    Lwt.catch
      (fun () -> Lwt.fail Pervasives.Exit)
      (function
        | Pervasives.Exit -> Lwt_io.printl "Got Pervasives.Exit"
        | exn -> Lwt.fail exn)
  end

*)

(*
let () =
  Lwt_main.run begin
    Lwt.bind Lwt_io.(read_line stdin) (fun line ->
        Lwt.bind (Lwt_unix.sleep 1.) (fun () ->
            Lwt_io.printf "One second ago, you entered %s\n" line))
  end
*)

(*
let () =
  Lwt_main.run begin
    let%lwt line = Lwt_io.(read_line stdin) in
    let%lwt () = Lwt_unix.sleep 1. in
    Lwt_io.printf "One second ago, you entered %s\n" line
  end
*)

(*
open Lwt.Infix

let () =
  Lwt_main.run begin
    Lwt_io.(read_line stdin) >>= fun line ->
    Lwt_unix.sleep 1. >>= fun () ->
    Lwt_io.printf "One second ago, you entered %s\n" line
  end
*)

(* 
(*val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t *)

let () =
  Lwt_main.run begin
    Lwt.catch
      (fun () -> Lwt.fail Pervasives.Exit)
      (function
        | Pervasives.Exit -> Lwt_io.printl "pog "
        | exn -> Lwt.fail exn)
  end
*)

(*
let () =
  Lwt_main.run begin
    try Lwt.fail Pervasives.Exit
    with Pervasives.Exit -> Lwt_io.printl "Got Pervasives.Exit"
  end
*)


(*
(* val finalize : (unit -> 'a t) -> (unit -> unit t) -> 'a t *)

let () =
  Lwt_main.run begin
    let%lwt file = Lwt_io.(open_file Input "bin/main.ml") in
    Lwt.finalize
      (fun () ->
         let%lwt content = Lwt_io.read file in
         Lwt_io.print content)
      (fun () ->
         Lwt_io.close file)
  end
*)

(* let () =
   let rec show_nag () : _ Lwt.t =
    let%lwt () = Lwt_io.printl "Please enter a line" in
    let%lwt () = Lwt_unix.sleep 1. in
    show_nag ()
   in
   Lwt.async (fun () -> show_nag ());

   Lwt_main.run begin
    let%lwt line = Lwt_io.(read_line stdin) in
    Lwt_io.printl line
   end
*)

(*
let () =
  let p_1 =
    let%lwt () = Lwt_unix.sleep 3. in
    Lwt_io.printl "Three seconds elapsed"
  in

  let p_2 =
    let%lwt () = Lwt_unix.sleep 5. in
    Lwt_io.printl "Five seconds elapsed"
  in

  let p_3 = Lwt.join [p_1; p_2] in
  Lwt_main.run p_3
*)
(*
let () =
  let echo =
    let%lwt line = Lwt_io.(read_line stdin) in
    Lwt_io.printl line
  in

  let timeout = Lwt_unix.sleep 25. in

  Lwt_main.run (Lwt.nchoose [echo; timeout])
*)
(*
let apply_print f g = f (g)

let () = 
  let echo = [print_endline; print_endline] in
  List.map apply_print echo
*)


let () =
  let request =
    let%lwt addresses = Lwt_unix.getaddrinfo "github.com" "80" [] in
    let google = Lwt_unix.((List.hd addresses).ai_addr) in

    Lwt_io.(with_connection google (fun (incoming, outgoing) ->
        let%lwt () = write outgoing "GET / HTTP/1.1\r\n" in
        let%lwt () = write outgoing "Connection: close\r\n\r\n" in
        let%lwt response = read incoming in
        Lwt.return (Some response)))
  in

  let timeout =
    let%lwt () = Lwt_unix.sleep 15. in
    Lwt.return None
  in

  match Lwt_main.run (Lwt.pick [request; timeout]) with
  | Some response -> print_string response
  | None -> prerr_endline "Request timed out"; exit 1

(* ocamlfind opt -package lwt.unix -package lwt_ppx -linkpkg -o request example.ml
   ./request *)