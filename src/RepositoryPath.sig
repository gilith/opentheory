(* ========================================================================= *)
(* PACKAGE DIRECTORY PATHS                                                   *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature DirectoryPath =
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
(* The package staging directory.                                            *)
(* ------------------------------------------------------------------------- *)

val mkStagingPackagesDirectory :
    {rootDirectory : string} -> {directory : string}

val mkStagingPackageDirectory :
    {rootDirectory : string} -> PackageNameVersion.nameVersion ->
    {directory : string}

(* ------------------------------------------------------------------------- *)
(* The repos directory.                                                      *)
(* ------------------------------------------------------------------------- *)

val mkReposDirectory :
    {rootDirectory : string} -> {directory : string}

val mkRepoFilename :
    {rootDirectory : string} -> PackageName.name -> {filename : string}

(* ------------------------------------------------------------------------- *)
(* The repo upload script.                                                   *)
(* ------------------------------------------------------------------------- *)

val mkUploadUrl : {rootUrl : string} -> {url : string}

val mkStartUploadUrl : {rootUrl : string} -> {url : string}

val mkInstallUploadUrl : {rootUrl : string} -> {url : string}

val mkFinishUploadUrl : {rootUrl : string} -> {url : string}

val mkDeleteUploadUrl : {rootUrl : string} -> {url : string}

val mkStatusUploadUrl :
    {rootUrl : string} -> Checksum.checksum -> {url : string}

end
