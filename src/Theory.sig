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

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty : 'a theory

val append : 'a theory -> 'a theory -> 'a theory

val map : ('a -> 'b) -> 'a theory -> 'b theory

(* ------------------------------------------------------------------------- *)
(* Articles read by the theory.                                              *)
(* ------------------------------------------------------------------------- *)

val articles :
    Interpretation.interpretation -> 'a theory ->
    (Interpretation.interpretation * {filename : string}) list

(* ------------------------------------------------------------------------- *)
(* Imported theories.                                                        *)
(* ------------------------------------------------------------------------- *)

val imports : 'a theory -> 'a list

(* ------------------------------------------------------------------------- *)
(* Compiling theories to articles.                                           *)
(* ------------------------------------------------------------------------- *)

val toArticle :
    {savable : bool,
     known : Article.article,
     simulations : Simulation.simulations,
     importToArticle : 'a -> Article.article,
     interpretation : Interpretation.interpretation,
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
