(* ========================================================================= *)
(* REPOSITORY PATHS                                                          *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure RepositoryPath :> RepositoryPath =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val configFilename = "config"
and packagesDirectory = "packages"
and remoteArgumentValueSeparator = "="
and remoteArgumentSeparator = "&"
and remoteQuery = "?"
and remoteRepositoriesDirectory = "repos"
and remoteRepositoryDirectory = "opentheory"
and remoteSeparator = "/"
and remoteStatusUploadArgument = "upload"
and remoteUploadDeleteDirectory = "delete"
and remoteUploadDirectory = "upload"
and remoteUploadFinishDirectory = "finish"
and remoteUploadInstallDirectory = "install"
and remoteUploadStartDirectory = "start"
and stagedPackagesDirectory = "staging";

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun joinDirectoryDirectory {directory = dir} {directory = file} =
    let
      val dir = OS.Path.joinDirFile {dir = dir, file = file}
    in
      {directory = dir}
    end;

fun joinDirectoryFilename {directory = dir} {filename = file} =
    let
      val file = OS.Path.joinDirFile {dir = dir, file = file}
    in
      {filename = file}
    end;

fun joinUrlDirectory {url} {directory} =
    {url = url ^ directory ^ remoteSeparator};

fun joinUrlFilename {url} {filename} = {url = url ^ filename};

local
  fun joinArg url (n,v) = url ^ n ^ remoteArgumentValueSeparator ^ v;

  fun joinFirst {url} nv = joinArg (url ^ remoteQuery) nv;

  fun joinNext (nv,url) = joinArg (url ^ remoteArgumentSeparator) nv;
in
  fun joinUrlArguments url args =
      case args of
        [] => url
      | nv :: nvs => {url = List.foldl joinNext (joinFirst url nv) nvs};
end;

(* ------------------------------------------------------------------------- *)
(* The repository directory.                                                 *)
(* ------------------------------------------------------------------------- *)

fun mkRepositoryDirectory {rootDirectory} = {directory = rootDirectory};

fun mkRootUrl {rootUrl} = {url = rootUrl};

local
  val remoteRepositoryDir = {directory = remoteRepositoryDirectory};
in
  fun mkRepositoryUrl root =
      joinUrlDirectory (mkRootUrl root) remoteRepositoryDir;
end;

(* ------------------------------------------------------------------------- *)
(* The config file.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  val configFile = {filename = configFilename};
in
  fun mkConfigFilename root =
    joinDirectoryFilename (mkRepositoryDirectory root) configFile;
end;

(* ------------------------------------------------------------------------- *)
(* The list of installed packages.                                           *)
(* ------------------------------------------------------------------------- *)

local
  val installedFile =
      RepositoryChecksums.mkFilename PackageName.installedChecksums;
in
  fun mkInstalledFilename root =
      joinDirectoryFilename (mkRepositoryDirectory root) installedFile;

  fun mkInstalledUrl root =
      joinUrlFilename (mkRepositoryUrl root) installedFile;
end;

(* ------------------------------------------------------------------------- *)
(* The packages directory.                                                   *)
(* ------------------------------------------------------------------------- *)

local
  val packagesDir = {directory = packagesDirectory};
in
  fun mkPackagesDirectory root =
      joinDirectoryDirectory (mkRepositoryDirectory root) packagesDir;

  fun mkPackagesUrl root =
      joinUrlDirectory (mkRepositoryUrl root) packagesDir;
end;

(* ------------------------------------------------------------------------- *)
(* The staged packages directory.                                            *)
(* ------------------------------------------------------------------------- *)

local
  val stagedDir = {directory = stagedPackagesDirectory};
in
  fun mkStagedPackagesDirectory root =
      joinDirectoryDirectory (mkRepositoryDirectory root) stagedDir;

  fun mkStagedPackagesUrl root =
      joinUrlDirectory (mkRepositoryUrl root) stagedDir;
end;

(* ------------------------------------------------------------------------- *)
(* Package directories.                                                      *)
(* ------------------------------------------------------------------------- *)

local
  fun packageDir namever =
      {directory = PackageNameVersion.toString namever};
in
  fun mkPackageDirectory root namever =
      joinDirectoryDirectory
        (mkPackagesDirectory root)
        (packageDir namever);

  fun mkPackageUrl root namever =
      joinUrlDirectory
        (mkPackagesUrl root)
        (packageDir namever);

  fun mkStagedPackageDirectory root namever =
      joinDirectoryDirectory
        (mkStagedPackagesDirectory root)
        (packageDir namever);

  fun mkStagedPackageUrl root namever =
      joinUrlDirectory
        (mkStagedPackagesUrl root)
        (packageDir namever);
end;

(* ------------------------------------------------------------------------- *)
(* Package tarballs.                                                         *)
(* ------------------------------------------------------------------------- *)

local
  fun tarballFile namever = PackageTarball.mkFilename namever;
in
  fun mkTarballFilename root namever =
      joinDirectoryFilename
        (mkPackageDirectory root namever)
        (tarballFile namever);

  fun mkTarballUrl root namever =
      joinUrlFilename
        (mkPackageUrl root namever)
        (tarballFile namever);

  fun mkStagedTarballFilename root namever =
      joinDirectoryFilename
        (mkStagedPackageDirectory root namever)
        (tarballFile namever);

  fun mkStagedTarballUrl root namever =
      joinUrlFilename
        (mkStagedPackageUrl root namever)
        (tarballFile namever);
end;

(* ------------------------------------------------------------------------- *)
(* The remote repositories directory.                                        *)
(* ------------------------------------------------------------------------- *)

local
  val remoteDir = {directory = remoteRepositoriesDirectory};
in
  fun mkRemoteRepositoriesDirectory root =
      joinDirectoryDirectory (mkRepositoryDirectory root) remoteDir;

  fun mkRemoteRepositoriesUrl root =
      joinUrlDirectory (mkRepositoryUrl root) remoteDir;
end;

local
  fun remoteChecksumsFile repo = RepositoryChecksums.mkFilename repo;
in
  fun mkRemoteRepositoryChecksumsFilename root repo =
      joinDirectoryFilename
        (mkRemoteRepositoriesDirectory root)
        (remoteChecksumsFile repo);

  fun mkRemoteRepositoryChecksumsUrl root repo =
      joinUrlFilename
        (mkRemoteRepositoriesUrl root)
        (remoteChecksumsFile repo);
end;

(* ------------------------------------------------------------------------- *)
(* The repo upload script.                                                   *)
(* ------------------------------------------------------------------------- *)

local
  val uploadDir = {directory = remoteUploadDirectory};
in
  fun mkUploadUrl root = joinUrlDirectory (mkRootUrl root) uploadDir;
end;

local
  val startUploadDir = {directory = remoteUploadStartDirectory};
in
  fun mkStartUploadUrl root =
      joinUrlDirectory (mkUploadUrl root) startUploadDir;
end;

local
  val installUploadDir = {directory = remoteUploadInstallDirectory};
in
  fun mkInstallUploadUrl root =
      joinUrlDirectory (mkUploadUrl root) installUploadDir;
end;

local
  val finishUploadDir = {directory = remoteUploadFinishDirectory};
in
  fun mkFinishUploadUrl root =
      joinUrlDirectory (mkUploadUrl root) finishUploadDir;
end;

local
  val deleteUploadDir = {directory = remoteUploadDeleteDirectory};
in
  fun mkDeleteUploadUrl root =
      joinUrlDirectory (mkUploadUrl root) deleteUploadDir;
end;

local
  fun mkUploadStatusArg token =
      (remoteStatusUploadArgument, Checksum.toString token);
in
  fun mkUploadStatusUrl root token =
      joinUrlArguments (mkRootUrl root) [mkUploadStatusArg token];
end;

end
