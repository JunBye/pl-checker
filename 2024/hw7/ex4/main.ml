(*
 * SNU 4190.310 Programming Languages
 *
 * Main Interface for M
 *)

open M
open Pp


let run () =
  let print_m = ref false in
  let result_only = ref false in
  let src = ref "" in
  let _ =
    Arg.parse
      [("-pp", Arg.Set print_m, "Print M program")
      ;("-resonly", Arg.Set result_only, "Print clean result only")
      ]
      (fun x -> src := x)
      "Usage: ./run [<options>] <M file>"
  in
  let _ = Error.init () in
  let chan = if !src = "" then stdin else open_in !src in
  let lexbuf = Lexing.from_channel chan in
  let pgm = Parser.program Lexer.start lexbuf in
  if not !result_only then (
    if !print_m then (
      let _ = print_string "== Input Program ==\n" in
      let _ = M_Printer.print_exp pgm in
      print_newline()
    );
  );
  try M_Printer.print_typ (Simple_checker.check pgm) with
  M.TypeError msg -> print_endline (if !result_only then "TypeError" else "Type Checking Failed : " ^ msg)

let _ = run ()
