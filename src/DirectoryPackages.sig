(* ========================================================================= *)
(* INSTALLED PACKAGE DIRECTORY                                               *)
(* Copyright (c) 2010 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

signature DirectoryPackages =
sig

(* ------------------------------------------------------------------------- *)
(* A type of installed packages.                                             *)
(* ------------------------------------------------------------------------- *)

type packages

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk :
    {system : DirectorySystem.system,
     rootDirectory : string} -> packages

val size : packages -> int

(* ------------------------------------------------------------------------- *)
(* Looking up installed packages.                                            *)
(* ------------------------------------------------------------------------- *)

val peek :
    packages -> PackageNameVersion.nameVersion -> PackageInfo.info option

val get : packages -> PackageNameVersion.nameVersion -> PackageInfo.info

val member : PackageNameVersion.nameVersion -> packages -> bool

val checksum :
    packages -> PackageNameVersion.nameVersion -> Checksum.checksum option

(* ------------------------------------------------------------------------- *)
(* Installed package sets.                                                   *)
(* ------------------------------------------------------------------------- *)

val all : packages -> PackageNameVersionSet.set

val latest :  (* ~Empty (Latest - Subtheories) All *)
    packages -> PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Installed package versions.                                               *)
(* ------------------------------------------------------------------------- *)

val nameVersions :
    packages -> PackageName.name -> PackageNameVersionSet.set

val latestNameVersion :
    packages -> PackageName.name -> PackageNameVersion.nameVersion option

val isLatestNameVersion :
    packages -> PackageNameVersion.nameVersion -> bool

val getLatestNameVersion :
    packages -> PackageName.name -> PackageNameVersion.nameVersion

val warnLatestNameVersion :
    packages -> PackageName.name -> PackageNameVersion.nameVersion option

val warnLatestNameVersionList :
    packages -> PackageName.name list ->
    PackageNameVersion.nameVersion list option

val previousNameVersion :
    packages -> PackageNameVersion.nameVersion ->
    PackageNameVersion.nameVersion option

(* ------------------------------------------------------------------------- *)
(* Package authors.                                                          *)
(* ------------------------------------------------------------------------- *)

val knownAuthor :
    packages -> PackageAuthorSet.set -> PackageNameVersion.nameVersion ->
    bool

(* ------------------------------------------------------------------------- *)
(* Package theory.                                                           *)
(* ------------------------------------------------------------------------- *)

val emptyTheory : packages -> PackageNameVersion.nameVersion -> bool

(* ------------------------------------------------------------------------- *)
(* Package requirements.                                                     *)
(* ------------------------------------------------------------------------- *)

val requiresInstalled :
    packages -> PackageNameVersion.nameVersion ->
    bool

val requiredBy :
    packages -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val isRequired :
    packages -> PackageNameVersion.nameVersion ->
    bool

(* This function silently ignores required packages that are not installed *)

val requires :
    packages -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

(* These functions emit warnings if required packages are not installed *)

val requiresNameVersions :
    packages -> PackageName.name list ->
    PackageNameVersion.nameVersion list option

val requiresPackages :
    packages -> PackageName.name list ->
    PackageInfo.info list option

val requiresTheorems :
    packages -> PackageName.name list ->
    PackageTheorems.theorems list option

(* ------------------------------------------------------------------------- *)
(* Included packages.                                                        *)
(* ------------------------------------------------------------------------- *)

val includes :
    packages -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val includedBy :
    packages -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val isIncluded :
    packages -> PackageNameVersion.nameVersion ->
    bool

val includesRTC :
    packages -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

val includedByRTC :
    packages -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Subtheory packages.                                                       *)
(* ------------------------------------------------------------------------- *)

val subtheoriesInstalled :
    packages -> PackageNameVersion.nameVersion ->
    bool

val subtheoryOf :
    packages -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val isSubtheory :
    packages -> PackageNameVersion.nameVersion ->
    bool

val subtheoryOfRTC :
    packages -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

(* These functions silently ignore subtheory packages that are not installed *)

val subtheories :
    packages -> PackageNameVersion.nameVersion ->
    PackageNameVersionSet.set

val subtheoriesRTC :
    packages -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Arranging packages in dependency order.                                   *)
(* ------------------------------------------------------------------------- *)

val includeOrder :
    packages -> PackageNameVersionSet.set ->
    PackageNameVersion.nameVersion list

val includeOrdered :
    packages -> PackageNameVersion.nameVersion list ->
    bool

val dependencyOrder :  (* Includes | Requires *)
    packages -> PackageNameVersionSet.set ->
    PackageNameVersion.nameVersion list

val dependencyOrdered :
    packages -> PackageNameVersion.nameVersion list ->
    bool

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val closedDependencies :
    packages -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

val acyclicDependencies :
    packages -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

val wellFoundedDependencies :
    packages -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Adding a new package.                                                     *)
(* ------------------------------------------------------------------------- *)

val add : packages -> PackageInfo.info -> Checksum.checksum -> unit

(* ------------------------------------------------------------------------- *)
(* Deleting a package.                                                       *)
(* ------------------------------------------------------------------------- *)

val delete : packages -> PackageNameVersion.nameVersion -> unit

(* ------------------------------------------------------------------------- *)
(* Comparing packages with repos.                                            *)
(* ------------------------------------------------------------------------- *)

val identicalOnRepo :
    packages -> DirectoryRepo.repo -> PackageNameVersion.nameVersion ->
    bool

val consistentWithRepo :
    packages -> DirectoryRepo.repo -> PackageNameVersion.nameVersion ->
    bool

val earlierThanRepo :
    packages -> DirectoryRepo.repo -> PackageNameVersion.nameVersion ->
    bool

val laterThanRepo :
    packages -> DirectoryRepo.repo -> PackageNameVersion.nameVersion ->
    bool

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : packages Print.pp

val toString : packages -> string

end
