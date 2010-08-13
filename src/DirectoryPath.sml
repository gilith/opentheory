(* ========================================================================= *)
(* PACKAGE DIRECTORY PATHS                                                   *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure DirectoryPath :> DirectoryPath =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val configFile = "config"
and installedName = "installed"
and directoryDirectory = "opentheory"
and packagesDirectory = "packages"
and stagingDirectory = "staging"
and repoSeparator = "/"
and reposDirectory = "repos";

(* ------------------------------------------------------------------------- *)
(* The directory of a repo.                                                  *)
(* ------------------------------------------------------------------------- *)

fun mkDirectoryUrl {rootUrl} =
      let
        val url = rootUrl ^ directoryDirectory ^ repoSeparator
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
      DirectoryChecksums.mkFilename installedName;
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
        val {url} = mkDirectoryUrl root

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
      val {url} = mkDirectoryUrl root

      val url = url ^ packagesDirectory ^ repoSeparator
    in
      {url = url}
    end;

fun mkPackageDirectory root name =
    let
      val {directory = dir} = mkPackagesDirectory root
      and file = PackageName.toString name

      val directory = OS.Path.joinDirFile {dir = dir, file = file}
    in
      {directory = directory}
    end;

fun mkPackageUrl root name =
    let
      val {url} = mkPackagesUrl root

      val url = url ^ PackageName.toString name ^ repoSeparator
    in
      {url = url}
    end;

fun mkTarballUrl root name =
    let
      val {url} = mkPackageUrl root name
      and {filename} = PackageInfo.mkTarball name

      val url = url ^ filename
    in
      {url = url}
    end;

(* ------------------------------------------------------------------------- *)
(* The package staging directory.                                            *)
(* ------------------------------------------------------------------------- *)

fun mkStagingPackagesDirectory {rootDirectory = dir} =
    let
      val directory = OS.Path.joinDirFile {dir = dir, file = stagingDirectory}
    in
      {directory = directory}
    end;

fun mkStagingPackageDirectory root name =
    let
      val {directory = dir} = mkStagingPackagesDirectory root
      and file = PackageName.toString name

      val directory = OS.Path.joinDirFile {dir = dir, file = file}
    in
      {directory = directory}
    end;

(* ------------------------------------------------------------------------- *)
(* The repos directory.                                                      *)
(* ------------------------------------------------------------------------- *)

fun mkReposDirectory {rootDirectory = dir} =
    let
      val directory = OS.Path.joinDirFile {dir = dir, file = reposDirectory}
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

end
