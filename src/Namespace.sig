(* ========================================================================= *)
(* OPENTHEORY NAMESPACES                                                     *)
(* Copyright (c) 2004-2008 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Namespace =
sig

(* ------------------------------------------------------------------------- *)
(* A type of namespaces.                                                     *)
(* ------------------------------------------------------------------------- *)

type namespace

(* ------------------------------------------------------------------------- *)
(* The top level namespace.                                                  *)
(* ------------------------------------------------------------------------- *)

val global : namespace

val isGlobal : namespace -> bool

(* ------------------------------------------------------------------------- *)
(* Nested namespaces (i.e., everything except the top level).                *)
(* ------------------------------------------------------------------------- *)

val mkNested : namespace * string -> namespace

val destNested : namespace -> namespace * string

val isNested : namespace -> bool

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

val compare : namespace * namespace -> order

val equal : namespace -> namespace -> bool

(* ------------------------------------------------------------------------- *)
(* Rewriting namespaces.                                                     *)
(* ------------------------------------------------------------------------- *)

val rewrite : namespace * namespace -> namespace -> namespace

(* ------------------------------------------------------------------------- *)
(* Parsing and pretty printing.                                              *)
(* ------------------------------------------------------------------------- *)

val toString : namespace -> string

val pp : namespace Parser.pp

val quotedToString : namespace -> string

val ppQuoted : namespace Parser.pp

val quotedParser : (char,namespace) Parser.parser

end
