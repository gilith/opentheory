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

val mkRepo : {name : string} -> repo

val nameRepo : repo -> string

val containsRepo : repo -> PackageName.name -> bool

val filenamesRepo : repo -> PackageName.name -> {filename : string} list option

val ppRepo : repo Print.pp

(* ------------------------------------------------------------------------- *)
(* Configuration.                                                            *)
(* ------------------------------------------------------------------------- *)

type config

val emptyConfig : config

val defaultConfig : config

val readConfig : {filename : string} -> config

val writeConfig : {config : config, filename : string} -> unit

val reposConfig : config -> repo list

val ppConfig : config Print.pp

(* ------------------------------------------------------------------------- *)
(* A type of theory package directories.                                     *)
(* ------------------------------------------------------------------------- *)

type directory

val create : {rootDirectory : string} -> directory

val mk : {rootDirectory : string} -> directory

val root : directory -> {directory : string}

val config : directory -> config

val repos : directory -> repo list

(* ------------------------------------------------------------------------- *)
(* Looking up packages in the package directory.                             *)
(* ------------------------------------------------------------------------- *)

val lookup : directory -> PackageFinder.finder

(* ------------------------------------------------------------------------- *)
(* Installing new packages into the package directory.                       *)
(* ------------------------------------------------------------------------- *)

val install : directory -> {filename : string} -> unit

(* ------------------------------------------------------------------------- *)
(* Uploading packages from the package directory to a repo.                  *)
(* ------------------------------------------------------------------------- *)

val upload : directory -> repo -> PackageName.name -> unit

(* ------------------------------------------------------------------------- *)
(* Downloading packages from a repo to the package directory.                *)
(* ------------------------------------------------------------------------- *)

val download : directory -> repo -> PackageName.name -> unit

end
