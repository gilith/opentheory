(* ========================================================================= *)
(* FINDING THEORY PACKAGES                                                   *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure PackageFinder :> PackageFinder =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theory package finders.                                         *)
(* ------------------------------------------------------------------------- *)

datatype finder =
    Finder of PackageNameVersion.nameVersion -> PackageInfo.info option;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk = Finder;

(* ------------------------------------------------------------------------- *)
(* Finding packages.                                                         *)
(* ------------------------------------------------------------------------- *)

fun find (Finder f) n = f n;

fun get f nv =
    case find f nv of
      SOME p => p
    | NONE =>
      let
        val err =
            "package " ^ PackageNameVersion.toString nv ^ " is not installed"
      in
        raise Error err
      end;

fun check f nv =
    let
      val _ = get f nv
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Finder combinators.                                                       *)
(* ------------------------------------------------------------------------- *)

val useless = mk (K NONE);

fun orelsef f1 f2 =
    let
      fun f nv =
          case find f1 nv of
            NONE => find f2 nv
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
