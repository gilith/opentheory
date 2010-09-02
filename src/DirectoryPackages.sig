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

val mk : {rootDirectory : string} -> packages

val size : packages -> int

(* ------------------------------------------------------------------------- *)
(* Looking up packages.                                                      *)
(* ------------------------------------------------------------------------- *)

val peek : packages -> PackageName.name -> PackageInfo.info option

val get : packages -> PackageName.name -> PackageInfo.info

val member : PackageName.name -> packages -> bool

val checksum : packages -> PackageName.name -> Checksum.checksum option

(* ------------------------------------------------------------------------- *)
(* All installed packages.                                                   *)
(* ------------------------------------------------------------------------- *)

val list : packages -> PackageNameSet.set

(* ------------------------------------------------------------------------- *)
(* Dependencies in the installed packages.                                   *)
(* ------------------------------------------------------------------------- *)

val parents : packages -> PackageName.name -> PackageNameSet.set

val children : packages -> PackageName.name -> PackageNameSet.set

val ancestors : packages -> PackageName.name -> PackageNameSet.set

val descendents : packages -> PackageName.name -> PackageNameSet.set

(* Sets *)

val ancestorsSet : packages -> PackageNameSet.set -> PackageNameSet.set

val descendentsSet : packages -> PackageNameSet.set -> PackageNameSet.set

(* ------------------------------------------------------------------------- *)
(* Generate a valid installation order.                                      *)
(* ------------------------------------------------------------------------- *)

val installOrder : packages -> PackageNameSet.set -> PackageName.name list

(* ------------------------------------------------------------------------- *)
(* Adding a new package.                                                     *)
(* ------------------------------------------------------------------------- *)

val add :
    DirectoryConfig.system ->
    packages -> PackageInfo.info -> Checksum.checksum -> unit

(* ------------------------------------------------------------------------- *)
(* Deleting a package.                                                       *)
(* ------------------------------------------------------------------------- *)

val delete : packages -> PackageName.name -> unit

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : packages Print.pp

val toString : packages -> string

end
