(* ========================================================================= *)
(* FINDING THEORY PACKAGES                                                   *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageFinder =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory package finders.                                         *)
(* ------------------------------------------------------------------------- *)

type finder

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk : (PackageName.name -> PackageInfo.info option) -> finder

(* ------------------------------------------------------------------------- *)
(* Finding packages.                                                         *)
(* ------------------------------------------------------------------------- *)

val find : finder -> PackageName.name -> PackageInfo.info option

val get : finder -> PackageName.name -> PackageInfo.info

val check : finder -> PackageName.name -> unit

(* ------------------------------------------------------------------------- *)
(* Finder combinators.                                                       *)
(* ------------------------------------------------------------------------- *)

val useless : finder

val orelsef : finder -> finder -> finder

val first : finder list -> finder

end
