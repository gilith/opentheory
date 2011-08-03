(* ========================================================================= *)
(* INTERPRETING OPENTHEORY NAMES                                             *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Interpretation =
sig

(* ------------------------------------------------------------------------- *)
(* A type of rewrite rules for names.                                        *)
(* ------------------------------------------------------------------------- *)

datatype rewrite =
    TypeOpRewrite of Name.name * Name.name
  | ConstRewrite of Name.name * Name.name

val compareRewrite : rewrite * rewrite -> order

val equalRewrite : rewrite -> rewrite -> bool

val ppRewrite : rewrite Print.pp

val ppRewriteList : rewrite list Print.pp

val toStringRewrite : rewrite -> string

val parserRewrite : (char,rewrite) Parse.parser

val parserRewriteList : (char, rewrite list) Parse.parser

(* ------------------------------------------------------------------------- *)
(* A type of interpretations.                                                *)
(* ------------------------------------------------------------------------- *)

type interpretation

val natural : interpretation

val toRewriteList : interpretation -> rewrite list

val fromRewriteList : rewrite list -> interpretation

val fromRewriteStream : rewrite Stream.stream -> interpretation

(* ------------------------------------------------------------------------- *)
(* Translating OpenTheory names.                                             *)
(* ------------------------------------------------------------------------- *)

val interpretTypeOp : interpretation -> Name.name -> Name.name

val interpretConst : interpretation -> Name.name -> Name.name

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : interpretation * interpretation -> order

val equal : interpretation -> interpretation -> bool

(* ------------------------------------------------------------------------- *)
(* Composing interpretations.                                                *)
(* ------------------------------------------------------------------------- *)

(* interpret (compose i1 i2) n = interpret i2 (interpret i1 n) *)

val compose : interpretation -> interpretation -> interpretation

(* ------------------------------------------------------------------------- *)
(* Restricting interpretations.                                              *)
(* ------------------------------------------------------------------------- *)

val restrict : Symbol.symbol -> interpretation -> interpretation

val restrictEqual : Symbol.symbol -> interpretation -> interpretation -> bool

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

val isCommentLine : char list -> bool

val toTextFile : {interpretation : interpretation, filename : string} -> unit

val fromTextFile : {filename : string} -> interpretation

end
