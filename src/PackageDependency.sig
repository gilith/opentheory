(* ========================================================================= *)
(* PACKAGE DEPENDENCY GRAPHS                                                 *)
(* Copyright (c) 2011 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature PackageDependency =
sig

(* ------------------------------------------------------------------------- *)
(* A type of package dependencies.                                           *)
(* ------------------------------------------------------------------------- *)

type dependency

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty : dependency

val size : dependency -> int

val member : PackageNameVersion.nameVersion -> dependency -> bool

(* ------------------------------------------------------------------------- *)
(* Adding package dependencies.                                              *)
(* ------------------------------------------------------------------------- *)

val add :
    {latest : PackageName.name -> PackageNameVersion.nameVersion option} ->
    dependency -> Package.package -> dependency

(* ------------------------------------------------------------------------- *)
(* Package requirements.                                                     *)
(* ------------------------------------------------------------------------- *)

val requiresInstalled :
    dependency -> PackageNameVersion.nameVersion ->
    bool

val requires :
    dependency -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val requiredBy :
    dependency -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val isRequired :
    dependency -> PackageNameVersion.nameVersion ->
    bool

(* ------------------------------------------------------------------------- *)
(* Included packages.                                                        *)
(* ------------------------------------------------------------------------- *)

val includes :
    dependency -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val includedBy :
    dependency -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val isIncluded :
    dependency -> PackageNameVersion.nameVersion ->
    bool

(* ------------------------------------------------------------------------- *)
(* Subtheory packages.                                                       *)
(* ------------------------------------------------------------------------- *)

val subtheoriesInstalled :
    dependency -> PackageNameVersion.nameVersion ->
    bool

val subtheories :
    dependency -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val subtheoryOf :
    dependency -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val isSubtheory :
    dependency -> PackageNameVersion.nameVersion ->
    bool

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val toString : dependency -> string

end
