(* ========================================================================= *)
(* PACKAGE VERSIONS                                                          *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageVersion =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory package versions.                                        *)
(* ------------------------------------------------------------------------- *)

type version

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : version * version -> order

val equal : version -> version -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : version Print.pp

val toString : version -> string

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parser : (char,version) Parse.parser

val fromString : string -> version

end
