(* ========================================================================= *)
(* ARTICLE FILE VERSIONS                                                     *)
(* Copyright (c) 2014 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature ArticleVersion =
sig

(* ------------------------------------------------------------------------- *)
(* A type of article file versions.                                          *)
(* ------------------------------------------------------------------------- *)

type version

val toInt : version -> int

val fromInt : int -> version

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : version * version -> order

val equal : version -> version -> bool

val earliest : version

val latest : version

(* ------------------------------------------------------------------------- *)
(* Supported commands.                                                       *)
(* ------------------------------------------------------------------------- *)

val supported : version -> Command.command -> bool

(* ------------------------------------------------------------------------- *)
(* The default version for reading articles.                                 *)
(* ------------------------------------------------------------------------- *)

val readDefault : version

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : version Print.pp

val toString : version -> string

end
