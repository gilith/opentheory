(* ========================================================================= *)
(* THEORY PACKAGE DIRECTORIES                                                *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Directory :> Directory =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val configFile = "config"
and openTheoryRepoUrl = "http://opentheory.gilith.com/"
and packagesDirectory = "packages"
and reposConfigSection = "repos"
and theoryExtension = "thy";

(* ------------------------------------------------------------------------- *)
(* Directories and filenames.                                                *)
(* ------------------------------------------------------------------------- *)

fun mkConfigFilename {rootDirectory} =
    OS.Path.joinDirFile {dir = rootDirectory, file = configFile};

fun mkPackagesDirectory {rootDirectory} =
    OS.Path.joinDirFile
      {dir = rootDirectory, file = packagesDirectory};

fun mkPackageDirectory root pkg =
    let
      val directory = mkPackagesDirectory root
    in
      OS.Path.joinDirFile
        {dir = directory, file = PackageName.toString pkg}
    end;

fun mkTheoryFilename pkg =
    OS.Path.joinBaseExt
      {base = PackageName.base pkg, ext = SOME theoryExtension};

(* ------------------------------------------------------------------------- *)
(* Repos.                                                                    *)
(* ------------------------------------------------------------------------- *)

datatype repo =
    Repo of
      {name : string,
       packages : {filename : string} list option PackageNameMap.map ref};

fun mkRepo {name} =
    let
      val packages = ref (PackageNameMap.new ())
    in
      Repo
        {name = name,
         packages = packages}
    end;

fun nameRepo (Repo {name = x, ...}) = x;

fun filesRepo repo pkg =
    let
      val Repo {packages, ...} = repo
      val ref pkgs = packages
    in
      case PackageNameMap.peek pkgs pkg of
        SOME pf => pf
      | NONE => raise Bug "Directory.filesRepo: web repos not implemented"
    end;

fun containsRepo repo pkg = Option.isSome (filesRepo repo pkg);

val ppRepo = Print.ppMap nameRepo Print.ppString;

val openTheoryRepo = mkRepo {name = openTheoryRepoUrl};

(* ------------------------------------------------------------------------- *)
(* Configuration.                                                            *)
(* ------------------------------------------------------------------------- *)

datatype config =
    Config of
      {repos : repo list};

val defaultConfig =
    let
      val repos = [openTheoryRepo]
    in
      Config {repos = repos}
    end;

val emptyConfig =
    let
      val repos = []
    in
      Config {repos = repos}
    end;

local
  val reposHandler =
      let
        fun beginSection config = config

        fun processLine (line,config) =
            let
              val Config {repos} = config

              val repo = mkRepo {name = line}

              val repos = repos @ [repo]
            in
              Config {repos = repos}
            end

        fun endSection config = config
      in
        Config.SectionHandler
          {beginSection = beginSection,
           processLine = processLine,
           endSection = endSection}
      end;

  fun sectionHandler section =
      if section = reposConfigSection then SOME reposHandler
      else NONE;

  val handler = Config.Handler sectionHandler;
in
  fun readConfig filename =
      Config.read handler emptyConfig filename
      handle IO.Io _ => emptyConfig;
end;

fun reposConfig (Config {repos = x, ...}) = x;

fun ppConfig conf =
    let
      val Config {repos = rs} = conf
    in
      Print.blockProgram Print.Consistent 0
        (Print.addString (Config.mkSectionHeader reposConfigSection) ::
         map (Print.sequence Print.addNewline o ppRepo) rs)
    end;

fun writeConfig {config,filename} =
    let
      val s = Print.toStream ppConfig config
    in
      Stream.toTextFile {filename = filename} s
    end;

(* ------------------------------------------------------------------------- *)
(* A type of theory package directories.                                     *)
(* ------------------------------------------------------------------------- *)

datatype directory =
    Directory of
      {rootDirectory : string,
       config : config,
       packages : Package.package option PackageNameMap.map ref};

fun newPackages () = ref (PackageNameMap.new ());

fun create {rootDirectory} =
    let
      val () = OS.FileSys.mkDir rootDirectory

      val () =
          let
            val dir = mkPackagesDirectory {rootDirectory = rootDirectory}
          in
            OS.FileSys.mkDir dir
          end

      val config = defaultConfig

      val () =
          let
            val filename = mkConfigFilename {rootDirectory = rootDirectory}
          in
            writeConfig {config = config, filename = filename}
          end

      val packages = newPackages ()
    in
      Directory
        {rootDirectory = rootDirectory,
         config = config,
         packages = packages}
    end;

fun mk {rootDirectory} =
    let
      val config =
          let
            val filename = mkConfigFilename {rootDirectory = rootDirectory}
          in
            readConfig {filename = filename}
          end

      val packages = newPackages ()
    in
      Directory
        {rootDirectory = rootDirectory,
         config = config,
         packages = packages}
    end;

fun root (Directory {rootDirectory = x, ...}) = {directory = x};

fun config (Directory {config = x, ...}) = x;

fun repos dir = reposConfig (config dir);

(* ------------------------------------------------------------------------- *)
(* Looking up packages in the package directory.                             *)
(* ------------------------------------------------------------------------- *)

local
  fun lookupPackage root pkg =
      let
        val directory = mkPackageDirectory root pkg

        val filename = mkTheoryFilename pkg

        val info =
            {name = SOME pkg,
             directory = directory,
             filename = filename}
      in
        SOME (Package.fromTextFile info)
        handle IO.Io _ => NONE
      end;

  fun lookupCached dir pkg =
      let
        val Directory {rootDirectory,packages,...} = dir

        val ref pkgs = packages
      in
        case PackageNameMap.peek pkgs pkg of
          SOME p => p
        | NONE =>
          let
            val mp = lookupPackage {rootDirectory = rootDirectory} pkg

            val pkgs = PackageNameMap.insert pkgs (pkg,mp)

            val () = packages := pkgs
          in
            mp
          end
      end;
in
  fun lookup dir = PackageFinder.mk (lookupCached dir);
end;

end
