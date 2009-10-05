(* ========================================================================= *)
(* THEORIES OF HIGHER ORDER LOGIC                                            *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Theory =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory syntax.                                                  *)
(* ------------------------------------------------------------------------- *)

datatype tag =
    Tag of
      {field : string,
       value : string}

datatype require =
    Require of
      {name : string,
       package : string,
       interpretation : Interpretation.interpretation,
       import : string list}

datatype theory =
    Local of theory * theory
  | Sequence of theory list
  | Article of {filename : string}
  | Interpret of Interpretation.interpretation * theory
  | Import of {require : string}

datatype file =
    File of
      {tags : tag list,
       requires : require list,
       theory : theory}

datatype instance =
    Instance of
      {package : string option,
       interpretation : Interpretation.interpretation,
       import : instance list,
       theory : theory}

and theory =
    Local of theory * theory
  | Sequence of theory list
  | Article of {filename : string}
  | Interpret of Interpretation.interpretation * theory
  | Import of {instance : instance}

val empty : theory

(* ------------------------------------------------------------------------- *)
(* Compiling theories to articles.                                           *)
(* ------------------------------------------------------------------------- *)

val toArticle : theory -> Article.article

val toSummary : theory -> Summary.summary

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : theory Print.pp

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parser : (char,theory) Parse.parser

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

val fromTextFile : {filename : string} -> theory

val toTextFile : {theory : theory, filename : string} -> unit

end
