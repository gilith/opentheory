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
(* All installed packages.                                                   *)
(* ------------------------------------------------------------------------- *)

val list : packages -> PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Looking up the latest version of packages.                                *)
(* ------------------------------------------------------------------------- *)

val latestVersion :
    packages -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

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

(* ------------------------------------------------------------------------- *)
(* Generate a valid installation order.                                      *)
(* ------------------------------------------------------------------------- *)

val installOrder :
    packages -> PackageNameVersionSet.set -> PackageNameVersion.nameVersion list

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
