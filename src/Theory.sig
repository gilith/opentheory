(* ========================================================================= *)
(* THEORIES OF HIGHER ORDER LOGIC                                            *)
(* Copyright (c) 2004-2009 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Theory =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theories.                                                       *)
(* ------------------------------------------------------------------------- *)

datatype theory =
    Local of theory * theory
  | Block of theory list
  | Article of {filename : string}
  | Interpretation of Interpretation.interpretation

val empty : theory

val toArticle : theory -> Article.article

val toSummary : theory -> Summary.summary

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

val fromTextFile : {filename : string} -> theory

val toTextFile : {theory : theory, filename : string} -> unit

end
