(* ========================================================================= *)
(* INTERPRETING OPENTHEORY NAMES                                             *)
(* Copyright (c) 2004-2008 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Interpretation =
sig

(* ------------------------------------------------------------------------- *)
(* A type of rewrite rules for names.                                        *)
(* ------------------------------------------------------------------------- *)

datatype rewrite =
    TypeOpRewrite of Name.name * Name.name
  | ConstRewrite of Name.name * Name.name

val interpretTypeOpRewrite : rewrite -> Name.name -> Name.name

val interpretConstRewrite : rewrite -> Name.name -> Name.name

val ppRewrite : rewrite Print.pp

val parserRewrite : (char,rewrite) Parse.parser

val toStringRewrite : rewrite -> string

(* ------------------------------------------------------------------------- *)
(* A type of interpretations.                                                *)
(* ------------------------------------------------------------------------- *)

type interpretation

val natural : interpretation

val singleton : rewrite -> interpretation

val fromRewrites : rewrite list -> interpretation

(* ------------------------------------------------------------------------- *)
(* Translating OpenTheory names.                                             *)
(* ------------------------------------------------------------------------- *)

val interpretTypeOp : interpretation -> Name.name -> Name.name

val interpretConst : interpretation -> Name.name -> Name.name

(* ------------------------------------------------------------------------- *)
(* Composing interpretations.                                                *)
(* ------------------------------------------------------------------------- *)

val compose : interpretation -> interpretation -> interpretation

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : interpretation Print.pp

val toString : interpretation -> string

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parser : (char,interpretation) Parse.parser

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

val toTextFile : {interpretation : interpretation, filename : string} -> unit

val fromTextFile : {filename : string} -> interpretation

end
