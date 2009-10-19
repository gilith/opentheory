(* ========================================================================= *)
(* THEORY PACKAGE DIRECTORIES                                                *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Directory =
sig

(* ------------------------------------------------------------------------- *)
(* Repos.                                                                    *)
(* ------------------------------------------------------------------------- *)

type repo

val containsRepo : repo -> PackageId.id -> bool

val filesRepo : repo -> PackageId.id -> string list option

(* ------------------------------------------------------------------------- *)
(* Configuration.                                                            *)
(* ------------------------------------------------------------------------- *)

type config

val mkConfig : {filename : string} -> config

val reposConfig : config -> repo list

(* ------------------------------------------------------------------------- *)
(* Packages.                                                                 *)
(* ------------------------------------------------------------------------- *)

type package

val directoryPackage : package -> {directory : string}

val filenamePackage : package -> {filename : string}

val contentsPackage : package -> Package.package

(* ------------------------------------------------------------------------- *)
(* A type of theory package directories.                                     *)
(* ------------------------------------------------------------------------- *)

type directory

val mk : {root : string} -> directory

val root : directory -> {directory : string}

val config : directory -> config

val repos : directory -> repo list

val lookup : directory -> PackageId.id -> package option

val download : directory -> PackageId.id -> directory * package

end
