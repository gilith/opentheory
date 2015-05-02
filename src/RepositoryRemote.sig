(* ========================================================================= *)
(* REMOTE REPOSITORIES                                                       *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature RepositoryRemote =
sig

(* ------------------------------------------------------------------------- *)
(* A type of remote repositories.                                            *)
(* ------------------------------------------------------------------------- *)

type name = PackageName.name

type remote

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk :
    {system : RepositorySystem.system,
     name : name,
     rootDirectory : string,
     rootUrl : string,
     upToDate : bool} -> remote

val name : remote -> name

val rootUrl : remote -> {rootUrl : string}

(* ------------------------------------------------------------------------- *)
(* Looking up packages.                                                      *)
(* ------------------------------------------------------------------------- *)

val peek : remote -> PackageNameVersion.nameVersion -> Checksum.checksum option

val member : PackageNameVersion.nameVersion -> remote -> bool

val first :
    remote list -> PackageNameVersion.nameVersion ->
    (remote * Checksum.checksum) option

val find :
    remote list -> PackageNameVersion.nameVersion * Checksum.checksum ->
    remote option

(* ------------------------------------------------------------------------- *)
(* Package versions.                                                         *)
(* ------------------------------------------------------------------------- *)

val previousNameVersion :
    remote -> PackageNameVersion.nameVersion ->
    (PackageNameVersion.nameVersion * Checksum.checksum) option

val previousNameVersionList :
    remote list -> PackageNameVersion.nameVersion ->
    (remote * PackageNameVersion.nameVersion * Checksum.checksum) option

val latestNameVersion :
    remote -> PackageName.name ->
    (PackageNameVersion.nameVersion * Checksum.checksum) option

val latestNameVersionList :
    remote list -> PackageName.name -> Checksum.checksum option ->
    (remote * PackageNameVersion.nameVersion * Checksum.checksum) option

val earlierThanLatestNameVersion :
    remote -> PackageNameVersion.nameVersion -> bool

val laterThanLatestNameVersion :
    remote -> PackageNameVersion.nameVersion -> bool

(* ------------------------------------------------------------------------- *)
(* Updating the package list.                                                *)
(* ------------------------------------------------------------------------- *)

val update : remote -> unit

(* ------------------------------------------------------------------------- *)
(* Downloading packages.                                                     *)
(* ------------------------------------------------------------------------- *)

val download : remote -> Package.package -> unit

(* ------------------------------------------------------------------------- *)
(* Uploading packages.                                                       *)
(* ------------------------------------------------------------------------- *)

type upload

val startUpload : remote -> upload

val supportUpload :
    upload -> PackageNameVersion.nameVersion -> Checksum.checksum -> unit

val packageUpload : upload -> Package.package -> unit

val finishUpload : upload -> unit

val deleteUpload : upload -> unit

val urlUpload : upload -> {url : string}

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : remote Print.pp

val toString : remote -> string

end
