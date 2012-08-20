(* ========================================================================= *)
(* THEORY PACKAGE DIRECTORIES                                                *)
(* Copyright (c) 2009 Joe Hurd, distributed under the MIT license            *)
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
(* Installed package sets.                                                   *)
(* ------------------------------------------------------------------------- *)

val all : directory -> PackageNameVersionSet.set

val latest :  (* ~Empty (Latest - Subtheories) All *)
    directory -> PackageNameVersionSet.set

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
(* Installed package versions.                                               *)
(* ------------------------------------------------------------------------- *)

val nameVersions :
    directory -> PackageName.name -> PackageNameVersionSet.set

val latestNameVersion :
    directory -> PackageName.name -> PackageNameVersion.nameVersion option

val isLatestNameVersion :
    directory -> PackageNameVersion.nameVersion -> bool

val getLatestNameVersion :
    directory -> PackageName.name -> PackageNameVersion.nameVersion

val warnLatestNameVersion :
    directory -> PackageName.name -> PackageNameVersion.nameVersion option

val warnLatestNameVersionList :
    directory -> PackageName.name list ->
    PackageNameVersion.nameVersion list option

val previousNameVersion :
    directory -> PackageNameVersion.nameVersion ->
    PackageNameVersion.nameVersion option

(* ------------------------------------------------------------------------- *)
(* Package authors.                                                          *)
(* ------------------------------------------------------------------------- *)

val knownAuthor :
    directory -> PackageAuthorSet.set -> PackageNameVersion.nameVersion ->
    bool

val selfAuthor :
    directory -> PackageNameVersion.nameVersion -> bool

(* ------------------------------------------------------------------------- *)
(* Package theory.                                                           *)
(* ------------------------------------------------------------------------- *)

val emptyTheory : directory -> PackageNameVersion.nameVersion -> bool

(* ------------------------------------------------------------------------- *)
(* Package requirements.                                                     *)
(* ------------------------------------------------------------------------- *)

val requiresInstalled :
    directory -> PackageNameVersion.nameVersion ->
    bool

val requiredBy :
    directory -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val isRequired :
    directory -> PackageNameVersion.nameVersion ->
    bool

(* This function silently ignores required packages that are not installed *)

val requires :
    directory -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

(* These functions emit warnings if required packages are not installed *)

val requiresNameVersions :
    directory -> PackageName.name list ->
    PackageNameVersion.nameVersion list option

val requiresPackages :
    directory -> PackageName.name list ->
    PackageInfo.info list option

val requiresTheorems :
    directory -> PackageName.name list ->
    PackageTheorems.theorems list option

(* ------------------------------------------------------------------------- *)
(* Included packages.                                                        *)
(* ------------------------------------------------------------------------- *)

val includes :
    directory -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val includedBy :
    directory -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val isIncluded :
    directory -> PackageNameVersion.nameVersion ->
    bool

val includesRTC :
    directory -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

val includedByRTC :
    directory -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Subtheory packages.                                                       *)
(* ------------------------------------------------------------------------- *)

val subtheoriesInstalled :
    directory -> PackageNameVersion.nameVersion ->
    bool

val subtheoryOf :
    directory -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val isSubtheory :
    directory -> PackageNameVersion.nameVersion ->
    bool

val subtheoryOfRTC :
    directory -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

(* These functions silently ignore subtheory packages that are not installed *)

val subtheories :
    directory -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val subtheoriesRTC :
    directory -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Arranging packages in dependency order.                                   *)
(* ------------------------------------------------------------------------- *)

val includeOrder :
    directory -> PackageNameVersionSet.set ->
    PackageNameVersion.nameVersion list

val includeOrdered :
    directory -> PackageNameVersion.nameVersion list ->
    bool

val dependencyOrder :  (* Requires | Includes *)
    directory -> PackageNameVersionSet.set ->
    PackageNameVersion.nameVersion list

val dependencyOrdered :
    directory -> PackageNameVersion.nameVersion list ->
    bool

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val closedDependencies :
    directory -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

val acyclicDependencies :
    directory -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

val wellFoundedDependencies :
    directory -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

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
(* Comparing packages with repos.                                            *)
(* ------------------------------------------------------------------------- *)

val identicalOnRepo :
    directory -> DirectoryRepo.repo -> PackageNameVersion.nameVersion ->
    bool

val consistentWithRepo :
    directory -> DirectoryRepo.repo -> PackageNameVersion.nameVersion ->
    bool

val earlierThanRepo :
    directory -> DirectoryRepo.repo -> PackageNameVersion.nameVersion ->
    bool

val laterThanRepo :
    directory -> DirectoryRepo.repo -> PackageNameVersion.nameVersion ->
    bool

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : directory Print.pp

end
