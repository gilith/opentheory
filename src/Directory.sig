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
(* A type of theory package directories.                                     *)
(* ------------------------------------------------------------------------- *)

type directory

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk : {rootDirectory : string} -> directory

val root : directory -> {directory : string}

val config : directory -> DirectoryConfig.config

val repos : directory -> DirectoryRepo.repo list

(* ------------------------------------------------------------------------- *)
(* Creating a new theory package directory.                                  *)
(* ------------------------------------------------------------------------- *)

val create : {rootDirectory : string} -> unit

(* ------------------------------------------------------------------------- *)
(* Looking up packages in the package directory.                             *)
(* ------------------------------------------------------------------------- *)

val peek : directory -> PackageName.name -> PackageInfo.info option

val get : directory -> PackageName.name -> PackageInfo.info

val member : directory -> PackageName.name -> bool

(* ------------------------------------------------------------------------- *)
(* Dependencies in the package directory.                                    *)
(* ------------------------------------------------------------------------- *)

val parents : directory -> PackageName.name -> PackageNameSet.set

val children : directory -> PackageName.name -> PackageNameSet.set

val ancestors : directory -> PackageName.name -> PackageNameSet.set

val descendents : directory -> PackageName.name -> PackageNameSet.set

(* Sets *)

val ancestorsSet : directory -> PackageNameSet.set -> PackageNameSet.set

val descendentsSet : directory -> PackageNameSet.set -> PackageNameSet.set

(* ------------------------------------------------------------------------- *)
(* Generate a valid installation order.                                      *)
(* ------------------------------------------------------------------------- *)

val installOrder : directory -> PackageNameSet.set -> PackageName.name list

(* ------------------------------------------------------------------------- *)
(* Listing packages in the package directory.                                *)
(* ------------------------------------------------------------------------- *)

val list : directory -> PackageNameSet.set

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

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : directory Print.pp

end
