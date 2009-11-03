(* ========================================================================= *)
(* FINDING THEORY PACKAGES                                                   *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageFinder :> PackageFinder =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theory package finders.                                         *)
(* ------------------------------------------------------------------------- *)

datatype finder =
    Finder of PackageName.name -> Package.package option;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk = Finder;

(* ------------------------------------------------------------------------- *)
(* Finding packages.                                                         *)
(* ------------------------------------------------------------------------- *)

fun find (Finder f) n = f n;

fun get f n =
    case find f n of
      SOME p => p
    | NONE => raise Error "PackageFinder.get";

(* ------------------------------------------------------------------------- *)
(* Finder combinators.                                                       *)
(* ------------------------------------------------------------------------- *)

val useless = mk (K NONE);

fun orelsef f1 f2 =
    let
      fun f n =
          case find f1 n of
            NONE => find f2 n
          | r as SOME _ => r
    in
      mk f
    end;

local
  fun inc (f2,f1) = orelsef f1 f2;
in
  fun first fl =
      case fl of
        [] => useless
      | f :: fl => List.foldl inc f fl;
end;

end
