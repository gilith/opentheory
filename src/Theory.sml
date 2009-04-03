(* ========================================================================= *)
(* THEORIES OF HIGHER ORDER LOGIC                                            *)
(* Copyright (c) 2004-2009 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Theory :> Theory =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theories.                                                       *)
(* ------------------------------------------------------------------------- *)

datatype theory =
    Local of theory * theory
  | Block of theory list
  | Article of {filename : string}
  | Interpretation of Interpretation.interpretation;

val empty = Block [];

fun compile {savable} thy = raise Bug "Theory.compile: not implemented";

fun toArticle thy = compile {savable = true} thy;

fun toSummary thy = Article.summarize (compile {savable = false} thy);

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun fromTextFile {filename} =
    let
    in
      raise Bug "Theory.fromTextFile: not implemented"
    end
    handle Error err => raise Error ("Theory.fromTextFile: " ^ err);

fun toTextFile {theory,filename} =
    let
    in
      raise Bug "Theory.toTextFile: not implemented"
    end
    handle Error err => raise Error ("Theory.toTextFile: " ^ err);

end
