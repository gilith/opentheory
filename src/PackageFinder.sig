(* ========================================================================= *)
(* FINDING THEORY PACKAGES                                                   *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
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

val mk :
    (PackageNameVersion.nameVersion -> Checksum.checksum option ->
     (Package.package * Checksum.checksum) option) -> finder

(* ------------------------------------------------------------------------- *)
(* Finding packages.                                                         *)
(* ------------------------------------------------------------------------- *)

val find :
    finder ->
    PackageNameVersion.nameVersion -> Checksum.checksum option ->
    (Package.package * Checksum.checksum) option

val get :
    finder ->
    PackageNameVersion.nameVersion -> Checksum.checksum option ->
    Package.package * Checksum.checksum

val check :
    finder ->
    PackageNameVersion.nameVersion -> Checksum.checksum option ->
    unit

(* ------------------------------------------------------------------------- *)
(* Finder combinators.                                                       *)
(* ------------------------------------------------------------------------- *)

val useless : finder

val orelsef : finder -> finder -> finder

val first : finder list -> finder

end
