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
    NamespaceRewrite of Namespace.namespace * Namespace.namespace
  | TypeRewrite of Name.name * Name.name
  | ConstRewrite of Name.name * Name.name
  | RulespaceRewrite of Namespace.namespace * Namespace.namespace
  | RuleRewrite of Name.name * Name.name

val interpretNamespaceRewrite :
    rewrite -> Namespace.namespace -> Namespace.namespace

val interpretTypeRewrite : rewrite -> Name.name -> Name.name

val interpretConstRewrite : rewrite -> Name.name -> Name.name

val interpretRulespaceRewrite :
    rewrite -> Namespace.namespace -> Namespace.namespace

val interpretRuleRewrite : rewrite -> Name.name -> Name.name

val ppRewrite : rewrite Print.pp

val parserRewrite : (char,rewrite) Parse.parser

val toStringRewrite : rewrite -> string

(* ------------------------------------------------------------------------- *)
(* A type of interpretations.                                                *)
(* ------------------------------------------------------------------------- *)

datatype interpretation = Interpretation of rewrite list

val natural : interpretation

val singleton : rewrite -> interpretation

val append : interpretation -> interpretation -> interpretation

val concat : interpretation list -> interpretation

(* ------------------------------------------------------------------------- *)
(* Translating OpenTheory names.                                             *)
(* ------------------------------------------------------------------------- *)

val interpretNamespace :
    interpretation -> Namespace.namespace -> Namespace.namespace

val interpretType : interpretation -> Name.name -> Name.name

val interpretConst : interpretation -> Name.name -> Name.name

val interpretRulespace :
    interpretation -> Namespace.namespace -> Namespace.namespace

val interpretRule : interpretation -> Name.name -> Name.name

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

val toTextFile : {filename : string, interpretation : interpretation} -> unit

val fromTextFile : {filename : string} -> interpretation

end
