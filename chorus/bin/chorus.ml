(* open Lwt.Infix *)
open Cmdliner


let version = "%%VERSION%%"

(* Help sections common to all commands *)
let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";

  `S "AUTHORS";
  `P "Humans Rio  <dev@rio.company>";

  `S "BUGS";
  `P "Check bug reports at https://github.com/rioadvancement/riomarket/issues.";
]

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())



let term_info title ~doc ~man =
  let man = man @ help_sections in
  Term.info ~sdocs:global_option_section ~doc ~man title

type command = unit Term.t * Term.info

type sub = {
  name: string;
  doc : string;
  man : Manpage.block list;
  term: unit Term.t;
}

let create_command c =
  let man = [
    `S "DESCRIPTION";
    `P c.doc;
  ] @ c.man in
  c.term, term_info c.name ~doc:c.doc ~man

let chorusconfig_man =
  let version_string = Printf.sprintf "Chorus %s" version in
  ("chorusconfig", 5, "", version_string, "Chorus Manual"), [
    `S Manpage.s_name;
    `P "irminconfig - Specify certain command-line options to save on typing";

    `S Manpage.s_synopsis;
    `P ".irminconfig";

    `S Manpage.s_description;
    `P "An $(b,irminconfig) file lets the user specify repetitve command-line options \
        in a text file. The $(b,irminconfig) file is only read if it is found in \
        the current working directory. Every line is of the form $(i,key)=$(i,value), \
        where $(i,key) is one of the following: $(b,contents), $(b,store), $(b,branch), \
        $(b,root), $(b,bare), $(b,head), or $(b,uri). These correspond to the irmin \
        options of the same names.";

    `S "NOTES";
    `P "When specifying a value for the $(b,contents) or $(b,store) options, the \
        shortest unique substring starting from index 0 is sufficient. For example, \
        \"store=g\" is equivalent to \"store=git\".";

    `S Manpage.s_examples;
    `P "Here is an example $(b,irminconfig) for accessing a local http irmin store. This \
        $(b,irminconfig) prevents the user from having to specify the $(b,store) and $(b,uri) \
        options for every command.";
    `Pre "    \\$ cat .irminconfig\n    store=http\n    uri=http://127.0.0.1:8080";
  ] @ help_sections


let mk (fn:'a): 'a Term.t =
  Term.(pure (fun () -> fn) $ setup_log)

(* HELP *)
let help = {
  name = "help";
  doc  = "Display help about Chorus and Chorus commands.";
  man = [
    `P "Use `$(mname) help topics' to get the full list of help topics.";
  ];
  term =
    let topic =
      let doc = Arg.info [] ~docv:"TOPIC" ~doc:"The topic to get help on." in
      Arg.(value & pos 0 (some string) None & doc )
    in
    let help man_format cmds topic = match topic with
      | None       -> `Help (`Pager, None)
      | Some topic ->
        let topics = "chorusconfig" :: cmds in
        let conv, _ = Arg.enum (List.rev_map (fun s -> (s, s)) ("topics" :: topics)) in
        match conv topic with
        | `Error e                -> `Error (false, e)
        | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
        | `Ok t when t = "chorusconfig" ->
          `Ok (Cmdliner.Manpage.print man_format Format.std_formatter chorusconfig_man)
        | `Ok t                   -> `Help (man_format, Some t) in
    Term.(ret (mk help $Term.man_format $Term.choice_names $topic))
}

let default =
  let doc = "Irmin, the database that never forgets." in
  let man = [
    `S "DESCRIPTION";
    `P "Irmin is a distributed database with built-in snapshot, branch \
        and revert mechanisms. It is designed to use a large variety of backends, \
        although it is optimized for append-only ones.";
    `P "Use either $(b,$(mname) <command> --help) or $(b,$(mname) help <command>) \
        for more information on a specific command.";
  ] in
  let usage () =
    Fmt.pr
      "usage: chorus [--version]\n\
      \             [--help]\n\
      \             <command> [<args>]\n\
       \n\
       The most commonly used subcommands are:\n\       
       \n\
       See `chorus help <command>` for more information on a specific command.\n\
       %!"      
  in
  Term.(mk usage $ pure ()),
  Term.info "irmin"
    ~version:version
    ~sdocs:global_option_section
    ~doc
    ~man

let commands = List.map create_command [
    help;   
  ]

let run ~default:x y =
  match Cmdliner.Term.eval_choice x y with
  | `Error _ -> exit 1
  | _        -> ()
