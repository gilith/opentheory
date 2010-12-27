(* ========================================================================= *)
(* THEORY PACKAGE META-DATA                                                  *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageInfo =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory package meta-data.                                       *)
(* ------------------------------------------------------------------------- *)

type info

val mk :
    {system : DirectorySystem.system,
     nameVersion : PackageNameVersion.nameVersion,
     directory : string} -> info

val nameVersion : info -> PackageNameVersion.nameVersion

val base : info -> PackageBase.base

val version : info -> PackageVersion.version

val directory : info -> {directory : string}

(* ------------------------------------------------------------------------- *)
(* Package directory operations.                                             *)
(* ------------------------------------------------------------------------- *)

val joinDirectory : info -> {filename : string} -> {filename : string}

val existsDirectory : info -> bool

val createDirectory : info -> unit

val nukeDirectory : info -> unit

(* ------------------------------------------------------------------------- *)
(* Is the package installed?                                                 *)
(* ------------------------------------------------------------------------- *)

val isInstalled : info -> bool

(* ------------------------------------------------------------------------- *)
(* The files needed by the package.                                          *)
(* ------------------------------------------------------------------------- *)

val theoryFile : info -> {filename : string}

val articleFiles : info -> {filename : string} list

val extraFiles : info -> PackageExtra.extra list

val allFiles : info -> {filename : string} list

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val packages : info -> PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Read the package.                                                         *)
(* ------------------------------------------------------------------------- *)

val package : info -> Package.package

(* ------------------------------------------------------------------------- *)
(* Package tarball.                                                          *)
(* ------------------------------------------------------------------------- *)

val tarball : info -> {filename : string}

val createTarball : info -> unit

val copyTarball : info -> {filename : string} -> unit

val downloadTarball : info -> {url : string} -> unit

val checksumTarball : info -> Checksum.checksum

val contentsTarball : info -> PackageTarball.contents

val unpackTarball : info -> PackageTarball.contents -> {minimal : bool} -> unit

val uploadTarball :
    info -> Checksum.checksum -> {url : string} -> {response : string}

(* ------------------------------------------------------------------------- *)
(* Package document.                                                         *)
(* ------------------------------------------------------------------------- *)

val document : info -> {filename : string}

val writeDocument : info -> PackageDocument.document -> unit

end
