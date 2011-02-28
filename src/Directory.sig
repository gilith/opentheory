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
(* Looking up acceptable licenses in the package directory.                  *)
(* ------------------------------------------------------------------------- *)

val licenses : directory -> DirectoryConfig.license list

val peekLicense :
    directory -> {name : string} -> DirectoryConfig.license option

val knownLicense : directory -> {name : string} -> bool

val getLicense : directory -> {name : string} -> DirectoryConfig.license

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

(* Set versions *)

val ancestorsSet :
    directory -> PackageNameVersionSet.set -> PackageNameVersionSet.set

val descendentsSet :
    directory -> PackageNameVersionSet.set -> PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Arranging packages in installation order.                                 *)
(* ------------------------------------------------------------------------- *)

val installOrder :
    directory -> PackageNameVersionSet.set ->
    PackageNameVersion.nameVersion list

val installOrdered : directory -> PackageNameVersion.nameVersion list -> bool

(* ------------------------------------------------------------------------- *)
(* Listing packages in the package directory.                                *)
(* ------------------------------------------------------------------------- *)

val list : directory -> PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Upgrading theory packages.                                                *)
(* ------------------------------------------------------------------------- *)

val checkUpgradeTheory :
    directory -> Package.package -> DirectoryError.error list

val upgradeTheory : directory -> Package.package -> Package.package option

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

val checkInstallStaged :
    directory -> PackageNameVersion.nameVersion -> Checksum.checksum ->
    DirectoryError.error list

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
    directory -> DirectoryRepo.repo -> PackageNameVersion.nameVersion list ->
    DirectoryError.error list

val packageUpload :
    directory -> DirectoryRepo.upload -> PackageNameVersion.nameVersion ->
    unit

(* ------------------------------------------------------------------------- *)
(* A package finder and importer.                                            *)
(* ------------------------------------------------------------------------- *)

val finder : directory -> PackageFinder.finder

val importer : directory -> Graph.importer

(* ------------------------------------------------------------------------- *)
(* A package finder for *staged* packages.                                   *)
(* ------------------------------------------------------------------------- *)

val stagedFinder : directory -> PackageFinder.finder

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : directory Print.pp

end
