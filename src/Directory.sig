(* ========================================================================= *)
(* THEORY PACKAGE DIRECTORIES                                                *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Directory =
sig

(* ------------------------------------------------------------------------- *)
(* Creating a new theory package directory.                                  *)
(* ------------------------------------------------------------------------- *)

val create : {rootDirectory : string} -> unit

(* ------------------------------------------------------------------------- *)
(* A type of theory package directories.                                     *)
(* ------------------------------------------------------------------------- *)

type directory

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk : {rootDirectory : string} -> directory

val rootDirectory : directory -> {rootDirectory : string}

val config : directory -> DirectoryConfig.config

(* ------------------------------------------------------------------------- *)
(* Looking up packages in the package directory.                             *)
(* ------------------------------------------------------------------------- *)

val peek : directory -> PackageName.name -> PackageInfo.info option

val get : directory -> PackageName.name -> PackageInfo.info

val member : directory -> PackageName.name -> bool

(* ------------------------------------------------------------------------- *)
(* Looking up repos in the package directory.                                *)
(* ------------------------------------------------------------------------- *)

val repos : directory -> DirectoryRepo.repo list

val peekRepo : directory -> DirectoryRepo.name -> DirectoryRepo.repo option

val getRepo : directory -> DirectoryRepo.name -> DirectoryRepo.repo

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
    directory ->
    PackageName.name -> Package.package ->
    DirectoryError.error list

val stageTheory :
    directory ->
    PackageName.name -> Package.package -> {directory : string} ->
    unit

(* ------------------------------------------------------------------------- *)
(* Staging theory packages for installation.                                 *)
(* ------------------------------------------------------------------------- *)

val checkStagePackage :
    directory ->
    DirectoryRepo.repo -> PackageName.name ->
    DirectoryError.error list

val stagePackage :
    directory -> PackageFinder.finder ->
    DirectoryRepo.repo -> PackageName.name ->
    unit

(* ------------------------------------------------------------------------- *)
(* Installing staged packages into the package directory.                    *)
(* ------------------------------------------------------------------------- *)

val installStaged : directory -> PackageName.name -> unit

(* ------------------------------------------------------------------------- *)
(* Uninstalling packages from the package directory.                         *)
(* ------------------------------------------------------------------------- *)

val checkUninstall :
    directory -> PackageName.name -> DirectoryError.error list

val uninstall : directory -> PackageName.name -> unit

(* ------------------------------------------------------------------------- *)
(* Uploading packages from the package directory to a repo.                  *)
(* ------------------------------------------------------------------------- *)

val checkUpload :
    directory ->
    DirectoryRepo.repo -> PackageName.name ->
    DirectoryError.error list

val upload :
    directory ->
    DirectoryRepo.repo -> PackageName.name ->
    unit

(* ------------------------------------------------------------------------- *)
(* A package finder.                                                         *)
(* ------------------------------------------------------------------------- *)

val finder : directory -> PackageFinder.finder

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : directory Print.pp

end
