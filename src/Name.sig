(* ========================================================================= *)
(* NAMES                                                                     *)
(* Copyright (c) 2004-2008 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Name =
sig

(* ------------------------------------------------------------------------- *)
(* A type of names.                                                          *)
(* ------------------------------------------------------------------------- *)

type name

val mk : Namespace.namespace * string -> name
val dest : name -> Namespace.namespace * string

val mkGlobal : string -> name
val destGlobal : name -> string
val isGlobal : name -> bool

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

val compare : name * name -> order

val equal : name -> name -> bool

(* ------------------------------------------------------------------------- *)
(* Rewriting names.                                                          *)
(* ------------------------------------------------------------------------- *)

val rewrite : Namespace.namespace * Namespace.namespace -> name -> name

val replace : name * name -> name -> name

(* ------------------------------------------------------------------------- *)
(* Parsing and pretty printing.                                              *)
(* ------------------------------------------------------------------------- *)

val toString : name -> string

val pp : name Parser.pp

val parser : (char,name) Parser.parser

end
