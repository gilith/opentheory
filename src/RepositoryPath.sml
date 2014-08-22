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

val configFile = "config"
and configFileExtension = "txt"
and deleteDirectory = "delete"
and finishDirectory = "finish"
and installDirectory = "install"
and packagesDirectory = "installed"
and remoteRepositoryArgumentValueSeparator = "="
and remoteQuery = "?"
and remoteSeparator = "/"
and remoteRepositoriesDirectory = "remote"
and remoteRepositoryDirectory = "opentheory"
and stagedDirectory = "staged"
and startDirectory = "start"
and statusUploadArgument = "upload"
and uploadDirectory = "upload";

(* ------------------------------------------------------------------------- *)
(* The repository directory of a remote repository.                          *)
(* ------------------------------------------------------------------------- *)

fun mkRepositoryUrl {rootUrl} =
      let
        val url = rootUrl ^ remoteRepositoryDirectory ^ remoteSeparator
      in
        {url = url}
      end;

(* ------------------------------------------------------------------------- *)
(* The config file.                                                          *)
(* ------------------------------------------------------------------------- *)

fun mkConfigFilename {rootDirectory = dir} =
    let
      val file = configFile

      val filename = OS.Path.joinDirFile {dir = dir, file = file}
    in
      {filename = filename}
    end;

(* ------------------------------------------------------------------------- *)
(* The list of installed packages.                                           *)
(* ------------------------------------------------------------------------- *)

local
  val {filename = installedFilename} =
      DirectoryChecksums.mkFilename PackageName.installedChecksums;
in
  fun mkInstalledFilename {rootDirectory = rootDir} =
      let
        val filename =
            OS.Path.joinDirFile
              {dir = rootDir,
               file = installedFilename}
      in
        {filename = filename}
      end;

  fun mkInstalledUrl root =
      let
        val {url} = mkRepositoryUrl root

        val url = url ^ installedFilename
      in
        {url = url}
      end;
end;

(* ------------------------------------------------------------------------- *)
(* The packages directory.                                                   *)
(* ------------------------------------------------------------------------- *)

fun mkPackagesDirectory {rootDirectory = dir} =
    let
      val directory = OS.Path.joinDirFile {dir = dir, file = packagesDirectory}
    in
      {directory = directory}
    end;

fun mkPackagesUrl root =
    let
      val {url} = mkRepositoryUrl root

      val url = url ^ packagesDirectory ^ repoSeparator
    in
      {url = url}
    end;

fun mkPackageDirectory root namever =
    let
      val {directory = dir} = mkPackagesDirectory root
      and file = PackageNameVersion.toString namever

      val directory = OS.Path.joinDirFile {dir = dir, file = file}
    in
      {directory = directory}
    end;

fun mkPackageUrl root namever =
    let
      val {url} = mkPackagesUrl root

      val url = url ^ PackageNameVersion.toString namever ^ repoSeparator
    in
      {url = url}
    end;

fun mkTarballUrl root namever =
    let
      val {url} = mkPackageUrl root namever
      and {filename} = PackageTarball.mkFilename namever

      val url = url ^ filename
    in
      {url = url}
    end;

(* ------------------------------------------------------------------------- *)
(* The staged packages directory.                                            *)
(* ------------------------------------------------------------------------- *)

fun mkStagedPackagesDirectory {rootDirectory = dir} =
    let
      val directory = OS.Path.joinDirFile {dir = dir, file = stagedDirectory}
    in
      {directory = directory}
    end;

fun mkStagedPackageDirectory root namever =
    let
      val {directory = dir} = mkStagedPackagesDirectory root
      and file = PackageNameVersion.toString namever

      val directory = OS.Path.joinDirFile {dir = dir, file = file}
    in
      {directory = directory}
    end;

(* ------------------------------------------------------------------------- *)
(* The remote repositories directory.                                        *)
(* ------------------------------------------------------------------------- *)

fun mkReposDirectory {rootDirectory = dir} =
    let
      val directory =
          OS.Path.joinDirFile
            {dir = dir,
             file = remoteRepositoriesDirectory}
    in
      {directory = directory}
    end;

fun mkRepoFilename root name =
    let
      val {directory = dir} = mkReposDirectory root

      val {filename = file} = DirectoryChecksums.mkFilename name

      val filename = OS.Path.joinDirFile {dir = dir, file = file}
    in
      {filename = filename}
    end;

(* ------------------------------------------------------------------------- *)
(* The repo upload script.                                                   *)
(* ------------------------------------------------------------------------- *)

fun mkUploadUrl root =
    let
      val {rootUrl = url} = root

      val url = url ^ uploadDirectory ^ repoSeparator
    in
      {url = url}
    end;

fun mkStartUploadUrl root =
    let
      val {url} = mkUploadUrl root

      val url = url ^ startDirectory ^ repoSeparator
    in
      {url = url}
    end;

fun mkInstallUploadUrl root =
    let
      val {url} = mkUploadUrl root

      val url = url ^ installDirectory ^ repoSeparator
    in
      {url = url}
    end;

fun mkFinishUploadUrl root =
    let
      val {url} = mkUploadUrl root

      val url = url ^ finishDirectory ^ repoSeparator
    in
      {url = url}
    end;

fun mkDeleteUploadUrl root =
    let
      val {url} = mkUploadUrl root

      val url = url ^ deleteDirectory ^ repoSeparator
    in
      {url = url}
    end;

fun mkStatusUploadUrl root token =
    let
      val {rootUrl = url} = root

      val url =
          url ^ remoteQuery ^ statusUploadArgument ^
          remoteRepositoryArgumentValueSeparator ^ Checksum.toString token
    in
      {url = url}
    end;

end
