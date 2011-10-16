(* ========================================================================= *)
(* THEORY PACKAGE DIRECTORIES                                                *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Directory =
sig

(* ------------------------------------------------------------------------- *)
(* Creating a new theory package directory.                                  *)
(* ------------------------------------------------------------------------- *)

val create : {rootDirectory : string, config : DirectoryConfig.config} -> unit

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
(* Package versions.                                                         *)
(* ------------------------------------------------------------------------- *)

val latestVersion :
    directory -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

val isLatestVersion :
    directory -> PackageNameVersion.nameVersion -> bool

val nameVersions :
    directory -> PackageName.name -> PackageVersionSet.set

val latestNameVersion :
    directory -> PackageName.name -> PackageNameVersion.nameVersion option

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

(* Auxiliary packages *)

val auxiliaryParents :
    directory -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

val auxiliaryChildren :
    directory -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

val auxiliaryAncestors :
    directory -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

val auxiliaryDescendents :
    directory -> PackageNameVersion.nameVersion -> PackageNameVersionSet.set

val auxiliaryAncestorsSet :
    directory -> PackageNameVersionSet.set -> PackageNameVersionSet.set

val auxiliaryDescendentsSet :
    directory -> PackageNameVersionSet.set -> PackageNameVersionSet.set

val isAuxiliary : directory -> PackageNameVersion.nameVersion -> bool

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

val upgrade : directory -> Package.package -> Package.package option

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
    {tool : Html.inline list} ->
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
    {filename : string} -> PackageTarball.contents ->
    {tool : Html.inline list} ->
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
    (Summary.summary -> Sequent.sequent -> bool) option ->
    {tool : Html.inline list} ->
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

val listStaged :
    directory -> {maxAge : Time.time option} -> PackageNameVersionSet.set

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
    {repo : DirectoryRepo.repo,
     support : PackageNameVersion.nameVersion list,
     packages : PackageNameVersion.nameVersion list} ->
    DirectoryError.error list

val supportUpload :
    directory -> DirectoryRepo.upload -> PackageNameVersion.nameVersion ->
    unit

val packageUpload :
    directory -> DirectoryRepo.upload -> PackageNameVersion.nameVersion ->
    unit

val ppUpload :
    directory ->
    {repo : DirectoryRepo.repo,
     support : PackageNameVersion.nameVersion list,
     packages : PackageNameVersion.nameVersion list} Print.pp

(* ------------------------------------------------------------------------- *)
(* A package finder and importer.                                            *)
(* ------------------------------------------------------------------------- *)

val finder : directory -> PackageFinder.finder

val importer : directory -> TheoryGraph.importer

(* ------------------------------------------------------------------------- *)
(* A package finder for *staged* packages.                                   *)
(* ------------------------------------------------------------------------- *)

val stagedFinder : directory -> PackageFinder.finder

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : directory Print.pp

end
