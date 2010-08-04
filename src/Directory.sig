(* ========================================================================= *)
(* THEORY PACKAGE DIRECTORIES                                                *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Directory =
sig

(* ------------------------------------------------------------------------- *)
(* A type of directory operation errors.                                     *)
(* ------------------------------------------------------------------------- *)

datatype error =
    AlreadyInstalledError
  | FilenameClashError of
      {srcs : {name : string, filename : string option} list,
       dest : {filename : string}}
  | InstalledDescendentError of PackageName.name
  | NotInstalledError
  | UninstalledParentError of PackageName.name

val isAlreadyInstalledError : error -> bool

val removeAlreadyInstalledError : error list -> bool * error list

val destInstalledDescendentError : error -> PackageName.name option

val isInstalledDescendentError : error -> bool

val removeInstalledDescendentError :
    error list -> PackageName.name list * error list

val isFatalError : error -> bool

val existsFatalError : error list -> bool

val toStringError : error -> string

val toStringErrorList : error list -> string

(* ------------------------------------------------------------------------- *)
(* Configuration.                                                            *)
(* ------------------------------------------------------------------------- *)

type config

val emptyConfig : config

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

val repos : directory -> DirectoryRepo.repo list

val pp : directory Print.pp

(* ------------------------------------------------------------------------- *)
(* Looking up packages in the package directory.                             *)
(* ------------------------------------------------------------------------- *)

val peek : directory -> PackageName.name -> PackageInfo.info option

val get : directory -> PackageName.name -> PackageInfo.info

val installed : directory -> PackageName.name -> bool

(* ------------------------------------------------------------------------- *)
(* Dependencies in the package directory.                                    *)
(* ------------------------------------------------------------------------- *)

val sortByAge : directory -> PackageNameSet.set -> PackageName.name list

val parents : directory -> PackageName.name -> PackageNameSet.set

val children : directory -> PackageName.name -> PackageNameSet.set

val ancestors : directory -> PackageName.name -> PackageNameSet.set

val ancestorsSet : directory -> PackageNameSet.set -> PackageNameSet.set

val descendents : directory -> PackageName.name -> PackageNameSet.set

val ancestorsByAge : directory -> PackageName.name -> PackageName.name list

val descendentsByAge : directory -> PackageName.name -> PackageName.name list

(* ------------------------------------------------------------------------- *)
(* Listing packages in the package directory.                                *)
(* ------------------------------------------------------------------------- *)

val list : directory -> PackageNameSet.set

val listByAge : directory -> PackageName.name list

(* ------------------------------------------------------------------------- *)
(* Staging theory files for installation.                                    *)
(* ------------------------------------------------------------------------- *)

val checkStageTheory :
    directory -> PackageName.name -> Package.package -> error list

val stageTheory :
    directory ->
    PackageName.name -> Package.package -> {directory : string} -> unit

(* ------------------------------------------------------------------------- *)
(* Installing staged packages into the package directory.                    *)
(* ------------------------------------------------------------------------- *)

val installStaged : directory -> PackageName.name -> unit

(* ------------------------------------------------------------------------- *)
(* Uninstalling packages from the package directory.                         *)
(* ------------------------------------------------------------------------- *)

val checkUninstall : directory -> PackageName.name -> error list

val uninstall : directory -> PackageName.name -> unit

(* ------------------------------------------------------------------------- *)
(* Updating the local package list.                                          *)
(* ------------------------------------------------------------------------- *)

val updateLocal : directory -> unit

(* ------------------------------------------------------------------------- *)
(* Uploading packages from the package directory to a repo.                  *)
(* ------------------------------------------------------------------------- *)

val checkUpload :
    directory -> DirectoryRepo.repo -> PackageName.name -> error list

val upload :
    directory -> DirectoryRepo.repo -> PackageName.name -> unit

(* ------------------------------------------------------------------------- *)
(* Downloading packages from a repo to the package directory.                *)
(* ------------------------------------------------------------------------- *)

val download : directory -> DirectoryRepo.repo -> PackageName.name -> unit

(* ------------------------------------------------------------------------- *)
(* A package finder.                                                         *)
(* ------------------------------------------------------------------------- *)

val finder : directory -> PackageFinder.finder

end
