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
    RewriteNamespace of Namespace.namespace * Namespace.namespace
  | RewriteType of Name.name * Name.name
  | RewriteConst of Name.name * Name.name
  | RewriteRule of Name.name * Name.name

val rewriteNamespace : rewrite -> Namespace.namespace -> Namespace.namespace

val rewriteType : rewrite -> Name.name -> Name.name

val rewriteConst : rewrite -> Name.name -> Name.name

val rewriteRule : rewrite -> Name.name -> Name.name

val rewriteToString : rewrite -> string

val rewriteParser : (char,rewrite) Parse.parser

(* ------------------------------------------------------------------------- *)
(* A type of interpretations.                                                *)
(* ------------------------------------------------------------------------- *)

datatype interpretation = Interpretation of rewrite list

val natural : interpretation

val append : interpretation -> interpretation -> interpretation

(* ------------------------------------------------------------------------- *)
(* Translating OpenTheory names.                                             *)
(* ------------------------------------------------------------------------- *)

val interpretNamespace :
    interpretation -> Namespace.namespace -> Namespace.namespace

val interpretType : interpretation -> Name.name -> Name.name

val interpretConst : interpretation -> Name.name -> Name.name

val interpretRule : interpretation -> Name.name -> Name.name

(* ------------------------------------------------------------------------- *)
(* Parsing and pretty printing.                                              *)
(* ------------------------------------------------------------------------- *)

val toString : interpretation -> string

val toTextFile : {filename : string, interpretation : interpretation} -> unit

val fromTextFile : {filename : string} -> interpretation

end
