(* ========================================================================= *)
(* THEORIES OF HIGHER ORDER LOGIC                                            *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Theory =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory syntax.                                                  *)
(* ------------------------------------------------------------------------- *)

datatype 'a theory =
    Local of 'a theory * 'a theory
  | Sequence of 'a theory list
  | Article of {filename : string}
  | Interpret of Interpretation.interpretation * 'a theory
  | Import of 'a

val empty : 'a theory

val append : 'a theory -> 'a theory -> 'a theory

(* ------------------------------------------------------------------------- *)
(* Compiling theories to articles.                                           *)
(* ------------------------------------------------------------------------- *)

val toArticle :
    {savable : bool,
     simulations : ObjectRead.simulations,
     importToArticle : 'a -> Article.article,
     interpretation : Interpretation.interpretation,
     import : 'a list,
     directory : string,
     theory : 'a theory} ->
    Article.article

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : 'a Print.pp -> 'a theory Print.pp

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parser : (char,'a) Parse.parser -> (char, 'a theory) Parse.parser

end
