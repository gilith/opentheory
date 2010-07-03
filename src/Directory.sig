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

val mkRepo : {name : string, url : string} -> repo

val nameRepo : repo -> string

val containsRepo : repo -> PackageName.name -> bool

val filenamesRepo : repo -> PackageName.name -> {filename : string} list option

val ppRepo : repo Print.pp

(* ------------------------------------------------------------------------- *)
(* Configuration.                                                            *)
(* ------------------------------------------------------------------------- *)

type config

val emptyConfig : config

val reposConfig : config -> repo list

val readConfig : {filename : string} -> config

val writeConfig : {config : config, filename : string} -> unit

val ppConfig : config Print.pp

val defaultConfig : config

(* ------------------------------------------------------------------------- *)
(* A type of theory package directories.                                     *)
(* ------------------------------------------------------------------------- *)

type directory

val create : {rootDirectory : string} -> directory

val mk : {rootDirectory : string} -> directory

val root : directory -> {directory : string}

val config : directory -> config

val repos : directory -> repo list

val pp : directory Print.pp

(* ------------------------------------------------------------------------- *)
(* Looking up packages in the package directory.                             *)
(* ------------------------------------------------------------------------- *)

val lookup : directory -> PackageFinder.finder

val installed : directory -> PackageName.name -> bool

(* ------------------------------------------------------------------------- *)
(* Listing packages in the package directory.                                *)
(* ------------------------------------------------------------------------- *)

val list : directory -> PackageNameSet.set

(* ------------------------------------------------------------------------- *)
(* Check whether it is possible to install a new package.                    *)
(* ------------------------------------------------------------------------- *)

datatype errorInstall =
    DirectoryExistsInstall
  | UninstalledDependencyInstall of PackageName.name
  | NonemptyPathInstall of {filename : string}
  | NameClashInstall of {filename : string} list

val isDirectoryExistsInstall : errorInstall -> bool

val removeDirectoryExistsInstall :
    errorInstall list -> bool * errorInstall list

val fatalErrorInstall : errorInstall -> bool

val toStringErrorInstall : errorInstall -> string

val toStringErrorInstallList : errorInstall list -> string

val checkInstall :
    directory -> PackageName.name -> Package.package -> errorInstall list

(* ------------------------------------------------------------------------- *)
(* Installing new packages into the package directory.                       *)
(* ------------------------------------------------------------------------- *)

val install :
    directory ->
    PackageName.name -> Package.package -> {filename : string} -> unit

(* ------------------------------------------------------------------------- *)
(* Installing new packages from the package directory.                       *)
(* ------------------------------------------------------------------------- *)

val uninstall : directory -> PackageName.name -> unit

(* ------------------------------------------------------------------------- *)
(* Uploading packages from the package directory to a repo.                  *)
(* ------------------------------------------------------------------------- *)

val upload : directory -> repo -> PackageName.name -> unit

(* ------------------------------------------------------------------------- *)
(* Downloading packages from a repo to the package directory.                *)
(* ------------------------------------------------------------------------- *)

val download : directory -> repo -> PackageName.name -> unit

end
