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
and reposDirectory = "repos";

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

  fun mkInstalledUrl {rootUrl} =
      let
        val url = rootUrl ^ "/" ^ directoryDirectory ^ installedFilename
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

fun mkPackageDirectory root name =
    let
      val {directory = dir} = mkPackagesDirectory root
      and file = PackageName.toString name

      val directory = OS.Path.joinDirFile {dir = dir, file = file}
    in
      {directory = directory}
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
