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

val mk : {name : PackageName.name, directory : string} -> info

val name : info -> PackageName.name

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

val extraFiles : info -> Package.extraFile list

val allFiles : info -> {filename : string} list

(* ------------------------------------------------------------------------- *)
(* Read the package.                                                         *)
(* ------------------------------------------------------------------------- *)

val package : info -> Package.package

(* ------------------------------------------------------------------------- *)
(* Package tarball.                                                          *)
(* ------------------------------------------------------------------------- *)

val tarball : info -> {filename : string}

val createTarball : DirectoryConfig.system -> info -> unit

val copyTarball : DirectoryConfig.system -> info -> {filename : string} -> unit

val downloadTarball : DirectoryConfig.system -> info -> {url : string} -> unit

val checksumTarball : DirectoryConfig.system -> info -> Checksum.checksum

val contentsTarball : DirectoryConfig.system -> info -> PackageTarball.contents

val unpackTarball : DirectoryConfig.system -> info -> unit

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val packages : info -> PackageNameSet.set

end
