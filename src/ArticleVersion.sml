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
    case cmd of
      Command.DefineTypeOp => version >= 6
    | Command.DefineTypeOpLegacy => version = 5
    | Command.Pragma => version >= 6
    | Command.Version => version >= 6
    | _ => true;

(* ------------------------------------------------------------------------- *)
(* The default version used when reading articles with no version specified. *)
(* ------------------------------------------------------------------------- *)

val readDefault = earliest;

(* ------------------------------------------------------------------------- *)
(* The default version used to write articles.                               *)
(* ------------------------------------------------------------------------- *)

val writeDefault = latest;

(* ------------------------------------------------------------------------- *)
(* The version used for articles when installing packages from theory files. *)
(* ------------------------------------------------------------------------- *)

val install = writeDefault;

(* ------------------------------------------------------------------------- *)
(* The version used for caching package theorems.                            *)
(* ------------------------------------------------------------------------- *)

val theorems = readDefault;

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

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

fun fromString s =
    case Int.fromString s of
      NONE =>
      let
        val err =
            "article version is set to " ^ s ^ ", but must be an integer"
      in
        raise Error err
      end
    | SOME n => fromInt n;

end
