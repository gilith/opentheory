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

val system : directory -> DirectorySystem.system

(* ------------------------------------------------------------------------- *)
(* Looking up packages in the package directory.                             *)
(* ------------------------------------------------------------------------- *)

val peek :
    directory -> PackageNameVersion.nameVersion -> PackageInfo.info option

val get : directory -> PackageNameVersion.nameVersion -> PackageInfo.info

val member : PackageNameVersion.nameVersion -> directory -> bool

val checksum :
    directory -> PackageNameVersion.nameVersion -> Checksum.checksum option

(* ------------------------------------------------------------------------- *)
(* Looking up repos in the package directory.                                *)
(* ------------------------------------------------------------------------- *)

val repos : directory -> DirectoryRepo.repo list

val peekRepo : directory -> DirectoryRepo.name -> DirectoryRepo.repo option

val getRepo : directory -> DirectoryRepo.name -> DirectoryRepo.repo

(* ------------------------------------------------------------------------- *)
(* Dependencies in the package directory.                                    *)
(* ------------------------------------------------------------------------- *)

val parents :
    directory -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

val children :
    directory -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

val ancestors :
    directory -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

val descendents :
    directory -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

(* Sets *)

val ancestorsSet :
    directory -> PackageNameVersionSet.set -> PackageNameVersionSet.set

val descendentsSet :
    directory -> PackageNameVersionSet.set -> PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Generate a valid installation order.                                      *)
(* ------------------------------------------------------------------------- *)

val installOrder :
    directory -> PackageNameVersionSet.set ->
    PackageNameVersion.nameVersion list

(* ------------------------------------------------------------------------- *)
(* Listing packages in the package directory.                                *)
(* ------------------------------------------------------------------------- *)

val list : directory -> PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Staging packages for installation.                                        *)
(* ------------------------------------------------------------------------- *)

val checkStagePackage :
    directory ->
    DirectoryRepo.repo -> PackageNameVersion.nameVersion -> Checksum.checksum ->
    DirectoryError.error list

val stagePackage :
    directory -> PackageFinder.finder ->
    DirectoryRepo.repo -> PackageNameVersion.nameVersion -> Checksum.checksum ->
    {minimal : bool} ->
    unit

(* ------------------------------------------------------------------------- *)
(* Staging tarballs for installation.                                        *)
(* ------------------------------------------------------------------------- *)

val checkStageTarball :
    directory ->
    PackageTarball.contents ->
    DirectoryError.error list

val stageTarball :
    directory -> PackageFinder.finder ->
    {filename : string} -> PackageTarball.contents -> {minimal : bool} ->
    unit

(* ------------------------------------------------------------------------- *)
(* Staging theory files for installation.                                    *)
(* ------------------------------------------------------------------------- *)

val checkStageTheory :
    directory ->
    PackageNameVersion.nameVersion -> Package.package ->
    DirectoryError.error list

val stageTheory :
    directory ->
    PackageNameVersion.nameVersion -> Package.package -> {directory : string} ->
    Checksum.checksum

(* ------------------------------------------------------------------------- *)
(* Installing staged packages into the package directory.                    *)
(* ------------------------------------------------------------------------- *)

val installStaged :
    directory -> PackageNameVersion.nameVersion -> Checksum.checksum -> unit

(* ------------------------------------------------------------------------- *)
(* Cleaning up staged packages.                                              *)
(* ------------------------------------------------------------------------- *)

val cleanupStaged : directory -> PackageNameVersion.nameVersion -> unit

(* ------------------------------------------------------------------------- *)
(* Uninstalling packages from the package directory.                         *)
(* ------------------------------------------------------------------------- *)

val checkUninstall :
    directory -> PackageNameVersion.nameVersion ->
    DirectoryError.error list

val uninstall : directory -> PackageNameVersion.nameVersion -> unit

(* ------------------------------------------------------------------------- *)
(* Uploading packages from the package directory to a repo.                  *)
(* ------------------------------------------------------------------------- *)

val checkUpload :
    directory ->
    DirectoryRepo.repo -> PackageNameVersion.nameVersion ->
    DirectoryError.error list

val upload :
    directory ->
    DirectoryRepo.repo -> PackageNameVersion.nameVersion ->
    {response : string}

(* ------------------------------------------------------------------------- *)
(* A package finder.                                                         *)
(* ------------------------------------------------------------------------- *)

val finder : directory -> PackageFinder.finder

(* ------------------------------------------------------------------------- *)
(* A package importer.                                                       *)
(* ------------------------------------------------------------------------- *)

val importer : directory -> Graph.importer

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : directory Print.pp

end
