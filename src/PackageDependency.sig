(* ========================================================================= *)
(* PACKAGE DEPENDENCY GRAPHS                                                 *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageDependency =
sig

(* ------------------------------------------------------------------------- *)
(* A type of package dependencies.                                           *)
(* ------------------------------------------------------------------------- *)

type dependency

(* ------------------------------------------------------------------------- *)
(* Constructors.                                                             *)
(* ------------------------------------------------------------------------- *)

val empty : dependency

(* ------------------------------------------------------------------------- *)
(* Adding package dependencies.                                              *)
(* ------------------------------------------------------------------------- *)

val addInfo :
    (PackageName.name -> PackageNameVersion.nameVersion option) ->
    dependency -> PackageInfo.info -> dependency

(* ------------------------------------------------------------------------- *)
(* Querying package dependencies.                                            *)
(* ------------------------------------------------------------------------- *)

val includes :
    dependency -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val includedBy :
    dependency -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val requires :
    dependency -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val requiredBy :
    dependency -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val subtheories :
    dependency -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val subtheoryOf :
    dependency -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val toString : dependency -> string

end
