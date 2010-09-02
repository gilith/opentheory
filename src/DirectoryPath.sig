(* ========================================================================= *)
(* PACKAGE DIRECTORY PATHS                                                   *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
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
    {rootDirectory : string} -> PackageName.name -> {directory : string}

val mkTarballUrl :
    {rootUrl : string} -> PackageName.name -> {url : string}

(* ------------------------------------------------------------------------- *)
(* The package staging directory.                                            *)
(* ------------------------------------------------------------------------- *)

val mkStagingPackagesDirectory :
    {rootDirectory : string} -> {directory : string}

val mkStagingPackageDirectory :
    {rootDirectory : string} -> PackageName.name -> {directory : string}

(* ------------------------------------------------------------------------- *)
(* The repos directory.                                                      *)
(* ------------------------------------------------------------------------- *)

val mkReposDirectory :
    {rootDirectory : string} -> {directory : string}

val mkRepoFilename :
    {rootDirectory : string} -> string -> {filename : string}

(* ------------------------------------------------------------------------- *)
(* The repo upload script.                                                   *)
(* ------------------------------------------------------------------------- *)

val mkUploadUrl : {rootUrl : string} -> {url : string}

end
