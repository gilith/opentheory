(* ========================================================================= *)
(* PACKAGE DIRECTORY REPOSITORIES                                            *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature DirectoryRepo =
sig

(* ------------------------------------------------------------------------- *)
(* A type of repos.                                                          *)
(* ------------------------------------------------------------------------- *)

type name = string

type repo

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk : {name : name, rootDirectory : string, rootUrl : string} -> repo

val name : repo -> name

val rootUrl : repo -> {rootUrl : string}

(* ------------------------------------------------------------------------- *)
(* Looking up packages.                                                      *)
(* ------------------------------------------------------------------------- *)

val peek : repo -> PackageName.name -> Checksum.checksum option

val member : PackageName.name -> repo -> bool

val first : repo list -> PackageName.name -> (repo * Checksum.checksum) option

val find : repo list -> PackageName.name * Checksum.checksum -> repo option

(* ------------------------------------------------------------------------- *)
(* Updating the package list.                                                *)
(* ------------------------------------------------------------------------- *)

val update : repo -> unit

(* ------------------------------------------------------------------------- *)
(* Downloading packages.                                                     *)
(* ------------------------------------------------------------------------- *)

val download : DirectoryConfig.system -> repo -> PackageInfo.info -> unit

(* ------------------------------------------------------------------------- *)
(* Uploading packages.                                                       *)
(* ------------------------------------------------------------------------- *)

val upload : repo -> PackageInfo.info -> unit

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : repo Print.pp

val toString : repo -> string

end
