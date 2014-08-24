(* ========================================================================= *)
(* REPOSITORY PATHS                                                          *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature RepositoryPath =
sig

(* ------------------------------------------------------------------------- *)
(* The config file.                                                          *)
(* ------------------------------------------------------------------------- *)

val mkConfigFilename :
    {rootDirectory : string} -> {filename : string}

(* ------------------------------------------------------------------------- *)
(* The list of installed packages.                                           *)
(* ------------------------------------------------------------------------- *)

val mkInstalledFilename :
    {rootDirectory : string} -> {filename : string}

val mkInstalledUrl :
    {rootUrl : string} -> {url : string}

(* ------------------------------------------------------------------------- *)
(* The packages directory.                                                   *)
(* ------------------------------------------------------------------------- *)

val mkPackagesDirectory :
    {rootDirectory : string} -> {directory : string}

val mkPackageDirectory :
    {rootDirectory : string} -> PackageNameVersion.nameVersion ->
    {directory : string}

val mkTarballUrl :
    {rootUrl : string} -> PackageNameVersion.nameVersion -> {url : string}

(* ------------------------------------------------------------------------- *)
(* The staged packages directory.                                            *)
(* ------------------------------------------------------------------------- *)

val mkStagedPackagesDirectory :
    {rootDirectory : string} -> {directory : string}

val mkStagedPackageDirectory :
    {rootDirectory : string} -> PackageNameVersion.nameVersion ->
    {directory : string}

(* ------------------------------------------------------------------------- *)
(* The remote repositories directory.                                        *)
(* ------------------------------------------------------------------------- *)

val mkRemoteRepositoriesDirectory :
    {rootDirectory : string} -> {directory : string}

val mkRemoteRepositoryChecksumsFilename :
    {rootDirectory : string} -> PackageName.name -> {filename : string}

(* ------------------------------------------------------------------------- *)
(* The remote repository upload script.                                      *)
(* ------------------------------------------------------------------------- *)

val mkUploadUrl : {rootUrl : string} -> {url : string}

val mkStartUploadUrl : {rootUrl : string} -> {url : string}

val mkInstallUploadUrl : {rootUrl : string} -> {url : string}

val mkFinishUploadUrl : {rootUrl : string} -> {url : string}

val mkDeleteUploadUrl : {rootUrl : string} -> {url : string}

val mkUploadStatusUrl :
    {rootUrl : string} -> Checksum.checksum -> {url : string}

end
