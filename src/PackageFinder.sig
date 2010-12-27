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

val mk : (PackageNameVersion.nameVersion -> PackageInfo.info option) -> finder

(* ------------------------------------------------------------------------- *)
(* Finding packages.                                                         *)
(* ------------------------------------------------------------------------- *)

val find : finder -> PackageNameVersion.nameVersion -> PackageInfo.info option

val get : finder -> PackageNameVersion.nameVersion -> PackageInfo.info

val check : finder -> PackageNameVersion.nameVersion -> unit

(* ------------------------------------------------------------------------- *)
(* Finder combinators.                                                       *)
(* ------------------------------------------------------------------------- *)

val useless : finder

val orelsef : finder -> finder -> finder

val first : finder list -> finder

end
