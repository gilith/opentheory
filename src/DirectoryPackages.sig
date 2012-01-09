(* ========================================================================= *)
(* INSTALLED PACKAGE DIRECTORY                                               *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature DirectoryPackages =
sig

(* ------------------------------------------------------------------------- *)
(* A type of installed packages.                                             *)
(* ------------------------------------------------------------------------- *)

type packages

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk :
    {system : DirectorySystem.system,
     rootDirectory : string} -> packages

val size : packages -> int

(* ------------------------------------------------------------------------- *)
(* Looking up packages.                                                      *)
(* ------------------------------------------------------------------------- *)

val peek :
    packages -> PackageNameVersion.nameVersion -> PackageInfo.info option

val get : packages -> PackageNameVersion.nameVersion -> PackageInfo.info

val member : PackageNameVersion.nameVersion -> packages -> bool

val checksum :
    packages -> PackageNameVersion.nameVersion -> Checksum.checksum option

(* ------------------------------------------------------------------------- *)
(* Sets of installed packages.                                               *)
(* ------------------------------------------------------------------------- *)

val toSet : packages -> PackageNameVersionSet.set

val latestSet : packages -> PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Package versions.                                                         *)
(* ------------------------------------------------------------------------- *)

val nameVersions :
    packages -> PackageName.name -> PackageVersionSet.set

val latestVersion :
    packages -> PackageName.name -> PackageVersion.version option

val latestNameVersion :
    packages -> PackageName.name -> PackageNameVersion.nameVersion option

val isLatestNameVersion :
    packages -> PackageNameVersion.nameVersion -> bool

(* ------------------------------------------------------------------------- *)
(* Dependencies in the installed packages.                                   *)
(* ------------------------------------------------------------------------- *)

val parents :
    packages -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

val children :
    packages -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

val ancestors :
    packages -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

val descendents :
    packages -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

(* Set versions *)

val ancestorsSet :
    packages -> PackageNameVersionSet.set -> PackageNameVersionSet.set

val descendentsSet :
    packages -> PackageNameVersionSet.set -> PackageNameVersionSet.set

(* Auxiliary packages *)

val auxiliaryParents :
    packages -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

val auxiliaryChildren :
    packages -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

val auxiliaryAncestors :
    packages -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

val auxiliaryDescendents :
    packages -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

val auxiliaryAncestorsSet :
    packages -> PackageNameVersionSet.set -> PackageNameVersionSet.set

val auxiliaryDescendentsSet :
    packages -> PackageNameVersionSet.set -> PackageNameVersionSet.set

val isAuxiliary : packages -> PackageNameVersion.nameVersion -> bool

(* ------------------------------------------------------------------------- *)
(* Arranging packages in installation order.                                 *)
(* ------------------------------------------------------------------------- *)

val installOrder :
    packages -> PackageNameVersionSet.set ->
    PackageNameVersion.nameVersion list

val installOrdered :
    packages -> PackageNameVersion.nameVersion list -> bool

(* ------------------------------------------------------------------------- *)
(* Adding a new package.                                                     *)
(* ------------------------------------------------------------------------- *)

val add : packages -> PackageInfo.info -> Checksum.checksum -> unit

(* ------------------------------------------------------------------------- *)
(* Deleting a package.                                                       *)
(* ------------------------------------------------------------------------- *)

val delete : packages -> PackageNameVersion.nameVersion -> unit

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : packages Print.pp

val toString : packages -> string

end
