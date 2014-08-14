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
    Finder of
      PackageNameVersion.nameVersion -> Checksum.checksum option ->
      (Package.package * Checksum.checksum) option;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk = Finder;

(* ------------------------------------------------------------------------- *)
(* Finding packages.                                                         *)
(* ------------------------------------------------------------------------- *)

fun find (Finder f) nv c = f nv c;

fun get f nv c =
    case find f nv c of
      SOME p => p
    | NONE =>
      let
        val err = "couldn't find package " ^ PackageNameVersion.toString nv
      in
        raise Error err
      end;

fun check f nv c =
    let
      val _ = get f nv c
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Finder combinators.                                                       *)
(* ------------------------------------------------------------------------- *)

val useless = mk (K (K NONE));

fun orelsef f1 f2 =
    let
      fun f nv c =
          case find f1 nv c of
            NONE => find f2 nv c
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
