(* ========================================================================= *)
(* PACKAGE DEPENDENCY GRAPHS                                                 *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageDependency =
sig

(* ------------------------------------------------------------------------- *)
(* A type of package dependency graphs.                                      *)
(* ------------------------------------------------------------------------- *)

type dependency

(* ------------------------------------------------------------------------- *)
(* Constructors.                                                             *)
(* ------------------------------------------------------------------------- *)

val empty : dependency

(* ------------------------------------------------------------------------- *)
(* Adding package dependencies.                                              *)
(* ------------------------------------------------------------------------- *)

val addInfo : dependency -> PackageInfo.info -> dependency

(* ------------------------------------------------------------------------- *)
(* Dependencies in the installed packages.                                   *)
(* ------------------------------------------------------------------------- *)

val parents :
    dependency -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val children :
    dependency -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val ancestors :
    dependency -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val descendents :
    dependency -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

(* Set versions *)

val ancestorsSet :
    dependency -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

val descendentsSet :
    dependency -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Generate a valid installation order.                                      *)
(* ------------------------------------------------------------------------- *)

val installOrder :
    dependency -> PackageNameVersionSet.set ->
    PackageNameVersion.nameVersion list

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val toString : dependency -> string

end
