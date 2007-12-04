(* ========================================================================= *)
(* BEHOLD PROOF CHECKER (PART OF THE OPENTHEORY TOOLSET)                     *)
(*                                                                           *)
(* Copyright (c) 2004-2007 Joe Hurd                                          *)
(*                                                                           *)
(* OpenTheory is free software; you can redistribute it and/or modify        *)
(* it under the terms of the GNU General Public License as published by      *)
(* the Free Software Foundation; either version 2 of the License, or         *)
(* (at your option) any later version.                                       *)
(*                                                                           *)
(* Metis is distributed in the hope that it will be useful,                  *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *)
(* GNU General Public License for more details.                              *)
(*                                                                           *)
(* You should have received a copy of the GNU General Public License         *)
(* along with Metis; if not, write to the Free Software                      *)
(* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA *)
(* ========================================================================= *)

open Useful;

(* ------------------------------------------------------------------------- *)
(* The program name.                                                         *)
(* ------------------------------------------------------------------------- *)

val PROGRAM = "behold";

(* ------------------------------------------------------------------------- *)
(* Program options                                                           *)
(* ------------------------------------------------------------------------- *)

val INPUT_SYSTEM = ref "behold";

val OUTPUT_SYSTEM = ref "behold";

val SYSTEMS = ["behold","HOL4","ProofPower","hol-light"];

local
  open Useful Options;
in
  val specialOptions =
      [{switches = ["-i","--input"], arguments = ["SYSTEM"],
        description = "the system that produced the input article",
        processor =
          beginOpt (stringOpt endOpt) (fn _ => fn s => INPUT_SYSTEM := s)},
       {switches = ["-o","--output"], arguments = ["SYSTEM"],
        description = "the system that will consume the output article",
        processor =
          beginOpt (stringOpt endOpt) (fn _ => fn s => OUTPUT_SYSTEM := s)}];
end;

val version_string = PROGRAM^" v1.0\n";

val programOptions =
    {name = PROGRAM,
     version = version_string,
     header = "usage: "^PROGRAM^" [option ...] <article.in >article.out\n" ^
              "Normalizes an article of theorems.\n",
     footer = "where SYSTEM is one of {" ^ join "," SYSTEMS ^ "}\n",
     options = specialOptions @ Options.basicOptions};

fun exit x : unit = Options.exit programOptions x;
fun succeed () = Options.succeed programOptions;
fun fail mesg = Options.fail programOptions mesg;
fun usage mesg = Options.usage programOptions mesg;

val (opts,work) =
    Options.processOptions programOptions (CommandLine.arguments ());

val () = if null work then () else usage "too many arguments";

(* ------------------------------------------------------------------------- *)
(* Top level                                                                 *)
(* ------------------------------------------------------------------------- *)

val () =
let
  val article = Stream.fromTextFile {filename = "-"}
in
  succeed ()
end
handle Error s => die (PROGRAM^" failed:\n" ^ s)
     | Bug s => die ("BUG found in "^PROGRAM^" program:\n" ^ s);
