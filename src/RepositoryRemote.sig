(* ========================================================================= *)
(* PACKAGE DIRECTORY REPOSITORIES                                            *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature DirectoryRepo =
sig

(* ------------------------------------------------------------------------- *)
(* A type of repos.                                                          *)
(* ------------------------------------------------------------------------- *)

type name = PackageName.name

type repo

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk :
    {system : DirectorySystem.system,
     name : name,
     rootDirectory : string,
     rootUrl : string,
     upToDate : bool} -> repo

val name : repo -> name

val rootUrl : repo -> {rootUrl : string}

(* ------------------------------------------------------------------------- *)
(* Looking up packages.                                                      *)
(* ------------------------------------------------------------------------- *)

val peek : repo -> PackageNameVersion.nameVersion -> Checksum.checksum option

val member : PackageNameVersion.nameVersion -> repo -> bool

val first :
    repo list -> PackageNameVersion.nameVersion ->
    (repo * Checksum.checksum) option

val find :
    repo list -> PackageNameVersion.nameVersion * Checksum.checksum ->
    repo option

(* ------------------------------------------------------------------------- *)
(* Package versions.                                                         *)
(* ------------------------------------------------------------------------- *)

val previousNameVersion :
    repo -> PackageNameVersion.nameVersion ->
    (PackageNameVersion.nameVersion * Checksum.checksum) option

val latestNameVersion :
    repo -> PackageName.name ->
    (PackageNameVersion.nameVersion * Checksum.checksum) option

val latestNameVersionList :
    repo list -> PackageName.name -> Checksum.checksum option ->
    (repo * PackageNameVersion.nameVersion * Checksum.checksum) option

(* ------------------------------------------------------------------------- *)
(* Updating the package list.                                                *)
(* ------------------------------------------------------------------------- *)

val update : repo -> unit

(* ------------------------------------------------------------------------- *)
(* Downloading packages.                                                     *)
(* ------------------------------------------------------------------------- *)

val download : repo -> PackageInfo.info -> unit

(* ------------------------------------------------------------------------- *)
(* Uploading packages.                                                       *)
(* ------------------------------------------------------------------------- *)

type upload

val startUpload : repo -> upload

val supportUpload :
    upload -> PackageNameVersion.nameVersion -> Checksum.checksum -> unit

val packageUpload : upload -> PackageInfo.info -> Checksum.checksum -> unit

val finishUpload : upload -> unit

val deleteUpload : upload -> unit

val urlUpload : upload -> {url : string}

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : repo Print.pp

val toString : repo -> string

end
