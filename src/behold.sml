(* ========================================================================= *)
(* THE BEHOLD PROOF CHECKER                                                  *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* Structures                                                                *)
(* ------------------------------------------------------------------------- *)

open Useful HOL;

infixr ## |->

(* ------------------------------------------------------------------------- *)
(* The program name                                                          *)
(* ------------------------------------------------------------------------- *)

val PROGRAM = "behold";

(* ------------------------------------------------------------------------- *)
(* The trace system                                                          *)
(* ------------------------------------------------------------------------- *)

val () = trace_level := 0;
fun chatting l = tracing {module = PROGRAM, level = l};
fun chat s = (trace s; true);

(* ------------------------------------------------------------------------- *)
(* Program options                                                           *)
(* ------------------------------------------------------------------------- *)

val INPUT_SYSTEM = ref "behold";

val OUTPUT_SYSTEM = ref "behold";

val SYSTEMS = ["behold","HOL4","ProofPower","hol-light"];

local
  open Useful Options;
in
  val special_options : opt list =
      [{switches = ["-i","--input"], arguments = ["SYSTEM"],
        description = "the system that produced the input article",
        processor = begin_proc (string_proc end_proc)
        (fn _ => fn s => INPUT_SYSTEM := s)},
       {switches = ["-o","--output"], arguments = ["SYSTEM"],
        description = "the system that will consume the output article",
        processor = begin_proc (string_proc end_proc)
        (fn _ => fn s => OUTPUT_SYSTEM := s)}];
end;

val version_string = PROGRAM^" v1.0\n";

val program_options =
    {name    = PROGRAM,
     version = version_string,
     header  = "usage: "^PROGRAM^" [option ...] <article.in >article.out\n" ^
               "Normalizes an article of theorems.\n",
     footer  = "where SYSTEM is one of {" ^ join "," SYSTEMS ^ "}\n",
     options = special_options @ Options.basic_options};

fun succeed () = Options.succeed program_options;
fun fail mesg = Options.fail program_options mesg;
fun usage mesg = Options.usage program_options mesg;

val (opts,work) =
  Options.process_options program_options (CommandLine.arguments ());

val () = if null work then () else usage "too many arguments";

(* ------------------------------------------------------------------------- *)
(* Top level                                                                 *)
(* ------------------------------------------------------------------------- *)

val () =
let
  val article = Stream.from_textfile {filename = "-"}
in
  succeed ()
end
handle Error s => die (PROGRAM^" failed:\n" ^ s)
     | Bug s => die ("BUG found in "^PROGRAM^" program:\n" ^ s);
