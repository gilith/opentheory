(* ========================================================================= *)
(* OPENTHEORY NAMESPACES                                                     *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Namespace =
sig

(* ------------------------------------------------------------------------- *)
(* A type of namespaces.                                                     *)
(* ------------------------------------------------------------------------- *)

type namespace

val append : namespace -> namespace -> namespace

val toList : namespace -> string list

val fromList : string list -> namespace

val fromString : string -> namespace

(* ------------------------------------------------------------------------- *)
(* The top-level namespace.                                                  *)
(* ------------------------------------------------------------------------- *)

val global : namespace

val isGlobal : namespace -> bool

(* ------------------------------------------------------------------------- *)
(* Nested namespaces (i.e., everything except the top-level).                *)
(* ------------------------------------------------------------------------- *)

val mkNested : namespace * string -> namespace

val destNested : namespace -> namespace * string

val isNested : namespace -> bool

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

val compareComponent : string * string -> order

val compare : namespace * namespace -> order

val equal : namespace -> namespace -> bool

(* ------------------------------------------------------------------------- *)
(* Rewriting namespaces.                                                     *)
(* ------------------------------------------------------------------------- *)

val rewrite : namespace * namespace -> namespace -> namespace option

(* ------------------------------------------------------------------------- *)
(* The standard namespace.                                                   *)
(* ------------------------------------------------------------------------- *)

val bool : namespace

val list : namespace

val natural : namespace

val option : namespace

val pair : namespace

val set : namespace

(* ------------------------------------------------------------------------- *)
(* Parsing and pretty printing.                                              *)
(* ------------------------------------------------------------------------- *)

val pp : namespace Print.pp

val toString : namespace -> string

val toHtml : namespace -> Html.inline list

(* The following can be used for serializing and unserializing *)

val ppQuoted : namespace Print.pp

val quotedToString : namespace -> string

val quotedParser : (char,namespace) Parse.parser

end
