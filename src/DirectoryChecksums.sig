(* ========================================================================= *)
(* PACKAGE DIRECTORY CHECKSUMS                                               *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature DirectoryChecksums =
sig

(* ------------------------------------------------------------------------- *)
(* Checksums filenames.                                                      *)
(* ------------------------------------------------------------------------- *)

val mkFilename : string -> {filename : string}

val destFilename : {filename : string} -> string option

val isFilename : {filename : string} -> bool

(* ------------------------------------------------------------------------- *)
(* Creating a new package checksums file.                                    *)
(* ------------------------------------------------------------------------- *)

val create : {filename : string} -> unit

(* ------------------------------------------------------------------------- *)
(* A type of package directory checkums.                                     *)
(* ------------------------------------------------------------------------- *)

type checksums

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk :
    {system : DirectorySystem.system,
     filename : string,
     updateFrom : {url : string} option} -> checksums

val filename : checksums -> {filename : string}

(* ------------------------------------------------------------------------- *)
(* Looking up packages.                                                      *)
(* ------------------------------------------------------------------------- *)

val peek :
    checksums -> PackageNameVersion.nameVersion -> Checksum.checksum option

val member : PackageNameVersion.nameVersion -> checksums -> bool

(* ------------------------------------------------------------------------- *)
(* Adding a new package.                                                     *)
(* ------------------------------------------------------------------------- *)

val add :
    checksums -> PackageNameVersion.nameVersion * Checksum.checksum -> unit

(* ------------------------------------------------------------------------- *)
(* Deleting a package.                                                       *)
(* ------------------------------------------------------------------------- *)

val delete : checksums -> PackageNameVersion.nameVersion -> unit

(* ------------------------------------------------------------------------- *)
(* Updating the package list.                                                *)
(* ------------------------------------------------------------------------- *)

val update : checksums -> {url : string} -> unit

end
