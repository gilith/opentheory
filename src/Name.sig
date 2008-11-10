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
(* Fresh names.                                                              *)
(* ------------------------------------------------------------------------- *)

val newName : unit -> name

val newNames : int -> name list

val variantPrime : (name -> bool) -> name -> name

val variantNum : (name -> bool) -> name -> name

(* ------------------------------------------------------------------------- *)
(* Rewriting names.                                                          *)
(* ------------------------------------------------------------------------- *)

val rewrite : Namespace.namespace * Namespace.namespace -> name -> name

val replace : name * name -> name -> name

(* ------------------------------------------------------------------------- *)
(* Parsing and pretty printing.                                              *)
(* ------------------------------------------------------------------------- *)

val ppQuoted : name Print.pp

val quotedToString : name -> string

val quotedParser : (char,name) Parse.parser

(* Not inverses, just for interfacing to external tools *)

val pp : name Print.pp

val toString : name -> string

val fromString : string -> name

end
