(* ========================================================================= *)
(* THEORY PACKAGE META-DATA                                                  *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
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

(* ------------------------------------------------------------------------- *)
(* Package directory.                                                        *)
(* ------------------------------------------------------------------------- *)

val directory : info -> {directory : string}

val joinDirectory : info -> {filename : string} -> {filename : string}

val existsDirectory : info -> bool

val createDirectory : info -> unit

val nukeDirectory : info -> unit

(* ------------------------------------------------------------------------- *)
(* Is the package installed?                                                 *)
(* ------------------------------------------------------------------------- *)

val isInstalled : info -> bool

(* ------------------------------------------------------------------------- *)
(* Read the package.                                                         *)
(* ------------------------------------------------------------------------- *)

val package : info -> Package.package

(* ------------------------------------------------------------------------- *)
(* Package name.                                                             *)
(* ------------------------------------------------------------------------- *)

val name : info -> PackageName.name

val version : info -> PackageVersion.version

val nameVersion : info -> PackageNameVersion.nameVersion

(* ------------------------------------------------------------------------- *)
(* Package description.                                                      *)
(* ------------------------------------------------------------------------- *)

val description : info -> {description : string}

(* ------------------------------------------------------------------------- *)
(* Package author.                                                           *)
(* ------------------------------------------------------------------------- *)

val author : info -> PackageAuthor.author

(* ------------------------------------------------------------------------- *)
(* Package license.                                                          *)
(* ------------------------------------------------------------------------- *)

val license : info -> {license : string}

(* ------------------------------------------------------------------------- *)
(* Package requirements.                                                     *)
(* ------------------------------------------------------------------------- *)

val requires : info -> PackageName.name list

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

val includes : info -> PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Show.                                                                     *)
(* ------------------------------------------------------------------------- *)

val show : info -> Show.show

(* ------------------------------------------------------------------------- *)
(* Package theory.                                                           *)
(* ------------------------------------------------------------------------- *)

val theory : info -> PackageTheory.theory list

val emptyTheory : info -> bool

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
    info -> Checksum.checksum -> {url : string, token : string} ->
    {response : string}

(* ------------------------------------------------------------------------- *)
(* Package theorems.                                                         *)
(* ------------------------------------------------------------------------- *)

val theoremsFile : info -> {filename : string}

val theorems : info -> PackageTheorems.theorems

val writeTheorems : info -> PackageTheorems.theorems -> unit

(* ------------------------------------------------------------------------- *)
(* Package document.                                                         *)
(* ------------------------------------------------------------------------- *)

val documentFile : info -> {filename : string}

val writeDocument : info -> PackageDocument.document -> unit

end
