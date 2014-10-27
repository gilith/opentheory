(* ========================================================================= *)
(* ARTICLE FILE VERSIONS                                                     *)
(* Copyright (c) 2014 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure ArticleVersion :> ArticleVersion =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of article file versions.                                          *)
(* ------------------------------------------------------------------------- *)

type version = int;

fun toInt v = v;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare = Int.compare;

fun equal (v1 : version) v2 = v1 = v2;

val earliest = 5;

val latest = 6;

(* ------------------------------------------------------------------------- *)
(* Supported commands.                                                       *)
(* ------------------------------------------------------------------------- *)

fun supported version cmd =
    raise Bug "ArticleVersion.supported: not implemented";

(* ------------------------------------------------------------------------- *)
(* The default version for reading articles.                                 *)
(* ------------------------------------------------------------------------- *)

val readDefault = earliest;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppInt;

val toString = Print.toString pp;

(* ------------------------------------------------------------------------- *)
(* Converting an integer to an article file version.                         *)
(* ------------------------------------------------------------------------- *)

fun fromInt n =
    if n < earliest then
      let
        val err =
            "article version is set to " ^ toString n ^
            ", but must be at least " ^ toString earliest
      in
        raise Error err
      end
    else if n > latest then
      let
        val err =
            "article version is set to " ^ toString n ^
            ", but the latest supported version is " ^ toString latest
      in
        raise Error err
      end
    else
      n;

end
