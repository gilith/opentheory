(* ========================================================================= *)
(* PACKAGE REPOSITORIES                                                      *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature Repository =
sig

(* ------------------------------------------------------------------------- *)
(* Creating a new theory package directory.                                  *)
(* ------------------------------------------------------------------------- *)

val create : {rootDirectory : string, config : RepositoryConfig.config} -> unit

(* ------------------------------------------------------------------------- *)
(* A type of package repositories.                                           *)
(* ------------------------------------------------------------------------- *)

type repository

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk : {rootDirectory : string} -> repository

val rootDirectory : repository -> {rootDirectory : string}

val config : repository -> RepositoryConfig.config

val system : repository -> RepositorySystem.system

(* ------------------------------------------------------------------------- *)
(* Looking up packages in the repository.                                    *)
(* ------------------------------------------------------------------------- *)

val peek :
    repository -> PackageNameVersion.nameVersion -> Package.package option

val get : repository -> PackageNameVersion.nameVersion -> Package.package

val member : PackageNameVersion.nameVersion -> repository -> bool

(* ------------------------------------------------------------------------- *)
(* Installed package sets.                                                   *)
(* ------------------------------------------------------------------------- *)

val all : repository -> PackageNameVersionSet.set

val latest :  (* ~Empty (Latest - Subtheories) All *)
    repository -> PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Looking up remote repositories known to the repository.                   *)
(* ------------------------------------------------------------------------- *)

val repos : repository -> RepositoryRepo.repo list

val peekRepo : repository -> RepositoryRepo.name -> RepositoryRepo.repo option

val getRepo : repository -> RepositoryRepo.name -> RepositoryRepo.repo

(* ------------------------------------------------------------------------- *)
(* Looking up licenses acceptable to the repository.                         *)
(* ------------------------------------------------------------------------- *)

val licenses : repository -> RepositoryConfig.license list

val peekLicense :
    repository -> {name : string} -> RepositoryConfig.license option

val knownLicense : repository -> {name : string} -> bool

val getLicense : repository -> {name : string} -> RepositoryConfig.license

(* ------------------------------------------------------------------------- *)
(* Installed package versions.                                               *)
(* ------------------------------------------------------------------------- *)

val nameVersions :
    repository -> PackageName.name -> PackageNameVersionSet.set

val latestNameVersion :
    repository -> PackageName.name -> PackageNameVersion.nameVersion option

val isLatestNameVersion :
    repository -> PackageNameVersion.nameVersion -> bool

val getLatestNameVersion :
    repository -> PackageName.name -> PackageNameVersion.nameVersion

val warnLatestNameVersion :
    repository -> PackageName.name -> PackageNameVersion.nameVersion option

val warnLatestNameVersionList :
    repository -> PackageName.name list ->
    PackageNameVersion.nameVersion list option

val previousNameVersion :
    repository -> PackageNameVersion.nameVersion ->
    PackageNameVersion.nameVersion option

(* ------------------------------------------------------------------------- *)
(* Package authors.                                                          *)
(* ------------------------------------------------------------------------- *)

val knownAuthor :
    repository -> PackageAuthorSet.set -> PackageNameVersion.nameVersion ->
    bool

val selfAuthor : repository -> PackageNameVersion.nameVersion -> bool

(* ------------------------------------------------------------------------- *)
(* Package theory.                                                           *)
(* ------------------------------------------------------------------------- *)

val emptyTheory : repository -> PackageNameVersion.nameVersion -> bool

(* ------------------------------------------------------------------------- *)
(* Package requirements.                                                     *)
(* ------------------------------------------------------------------------- *)

val requiresInstalled :
    repository -> PackageNameVersion.nameVersion ->
    bool

val requiredBy :
    repository -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val isRequired :
    repository -> PackageNameVersion.nameVersion ->
    bool

(* This function silently ignores required packages that are not installed *)

val requires :
    repository -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

(* These functions emit warnings if required packages are not installed *)

val requiresNameVersions :
    repository -> PackageName.name list ->
    PackageNameVersion.nameVersion list option

val requiresPackages :
    repository -> PackageName.name list ->
    PackageInfo.info list option

val requiresTheorems :
    repository -> PackageName.name list ->
    PackageTheorems.theorems list option

(* ------------------------------------------------------------------------- *)
(* Included packages.                                                        *)
(* ------------------------------------------------------------------------- *)

val includes :
    repository -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val includedBy :
    repository -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val isIncluded :
    repository -> PackageNameVersion.nameVersion ->
    bool

val includesRTC :
    repository -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

val includedByRTC :
    repository -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Subtheory packages.                                                       *)
(* ------------------------------------------------------------------------- *)

val subtheoriesInstalled :
    repository -> PackageNameVersion.nameVersion ->
    bool

val subtheoryOf :
    repository -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val isSubtheory :
    repository -> PackageNameVersion.nameVersion ->
    bool

val subtheoryOfRTC :
    repository -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

(* These functions silently ignore subtheory packages that are not installed *)

val subtheories :
    repository -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val subtheoriesRTC :
    repository -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Arranging packages in dependency order.                                   *)
(* ------------------------------------------------------------------------- *)

val includeOrder :
    repository -> PackageNameVersionSet.set ->
    PackageNameVersion.nameVersion list

val includeOrdered :
    repository -> PackageNameVersion.nameVersion list ->
    bool

val dependencyOrder :  (* Requires | Includes *)
    repository -> PackageNameVersionSet.set ->
    PackageNameVersion.nameVersion list

val dependencyOrdered :
    repository -> PackageNameVersion.nameVersion list ->
    bool

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val closedDependencies :
    repository -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

val acyclicDependencies :
    repository -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

val upToDateDependencies :
    repository -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Upgrading theory source files.                                            *)
(* ------------------------------------------------------------------------- *)

val upgrade : repository -> Package.package -> Package.package option

(* ------------------------------------------------------------------------- *)
(* Staging packages for installation.                                        *)
(* ------------------------------------------------------------------------- *)

val checkStagePackage :
    repository ->
    RepositoryRepo.repo -> PackageNameVersion.nameVersion -> Checksum.checksum ->
    RepositoryError.error list

val stagePackage :
    repository -> PackageFinder.finder ->
    RepositoryRepo.repo -> PackageNameVersion.nameVersion -> Checksum.checksum ->
    {tool : Html.inline list} ->
    unit

(* ------------------------------------------------------------------------- *)
(* Staging tarballs for installation.                                        *)
(* ------------------------------------------------------------------------- *)

val checkStageTarball :
    repository ->
    PackageTarball.contents ->
    RepositoryError.error list

val stageTarball :
    repository -> PackageFinder.finder ->
    {filename : string} -> PackageTarball.contents ->
    {tool : Html.inline list} ->
    unit

(* ------------------------------------------------------------------------- *)
(* Staging theory files for installation.                                    *)
(* ------------------------------------------------------------------------- *)

val checkStageTheory :
    repository ->
    PackageNameVersion.nameVersion -> Package.package ->
    RepositoryError.error list

val stageTheory :
    repository ->
    PackageNameVersion.nameVersion -> Package.package -> {repository : string} ->
    {tool : Html.inline list} ->
    Checksum.checksum

(* ------------------------------------------------------------------------- *)
(* Installing staged packages into the repository.                           *)
(* ------------------------------------------------------------------------- *)

val checkInstallStaged :
    repository -> PackageNameVersion.nameVersion -> Checksum.checksum ->
    RepositoryError.error list

val installStaged :
    repository -> PackageNameVersion.nameVersion -> Checksum.checksum -> unit

(* ------------------------------------------------------------------------- *)
(* Cleaning up staged packages.                                              *)
(* ------------------------------------------------------------------------- *)

val listStaged :
    repository -> {maxAge : Time.time option} -> PackageNameVersionSet.set

val cleanupStaged : repository -> PackageNameVersion.nameVersion -> unit

(* ------------------------------------------------------------------------- *)
(* Uninstalling packages from the repository.                                *)
(* ------------------------------------------------------------------------- *)

val checkUninstall :
    repository -> PackageNameVersion.nameVersion ->
    RepositoryError.error list

val uninstall : repository -> PackageNameVersion.nameVersion -> unit

(* ------------------------------------------------------------------------- *)
(* Uploading installed packages to a remote repository.                      *)
(* ------------------------------------------------------------------------- *)

type upload

val mkUpload :
    {repo : RepositoryRepo.repo,
     support : PackageNameVersion.nameVersion list,
     packages : PackageNameVersion.nameVersion list} ->

val checkUpload :
    repository ->
    {repo : RepositoryRepo.repo,
     support : PackageNameVersion.nameVersion list,
     packages : PackageNameVersion.nameVersion list} ->
    RepositoryError.error list

val supportUpload :
    repository -> RepositoryRepo.upload -> PackageNameVersion.nameVersion ->
    unit

val packageUpload :
    repository -> RepositoryRepo.upload -> PackageNameVersion.nameVersion ->
    unit

val ppUpload :
    repository ->
    {repo : RepositoryRepo.repo,
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
