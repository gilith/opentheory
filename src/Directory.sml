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
and nameRepoSectionKey = "name"
and openTheoryRepoName = "gilith"
and openTheoryRepoUrl = "http://opentheory.gilith.com/"
and packagesDirectory = "packages"
and repoConfigSection = "repo"
and theoryExtension = "thy"
and urlRepoSectionKey = "url";

(* ------------------------------------------------------------------------- *)
(* Directories and filenames.                                                *)
(* ------------------------------------------------------------------------- *)

fun ppDirectory {directory} = Print.ppString directory;

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
       url : string,
       packages : {filename : string} list option PackageNameMap.map ref};

fun mkRepo {name,url} =
    let
      val packages = ref (PackageNameMap.new ())
    in
      Repo
        {name = name,
         url = url,
         packages = packages}
    end;

fun nameRepo (Repo {name = x, ...}) = x;

fun urlRepo (Repo {url = x, ...}) = x;

fun filenamesRepo repo pkg =
    let
      val Repo {packages, ...} = repo
      val ref pkgs = packages
    in
      case PackageNameMap.peek pkgs pkg of
        SOME pf => pf
      | NONE => raise Bug "Directory.filesRepo: web repos not implemented"
    end;

fun containsRepo repo pkg = Option.isSome (filenamesRepo repo pkg);

val ppRepo = Print.ppMap nameRepo Print.ppString;

val openTheoryRepo =
    mkRepo
      {name = openTheoryRepoName,
       url = openTheoryRepoUrl};

(* ------------------------------------------------------------------------- *)
(* Configuration.                                                            *)
(* ------------------------------------------------------------------------- *)

datatype config =
    Config of
      {repos : repo list};

val emptyConfig =
    let
      val repos = []
    in
      Config {repos = repos}
    end;

fun reposConfig (Config {repos = x, ...}) = x;

fun addRepoConfig config r =
    let
      val Config {repos = rs} = config

      val rs = rs @ [r]
    in
      Config {repos = rs}
    end;

local
  local
    datatype repoSectionState =
        RepoSectionState of
          {name : string option,
           url : string option};

    val initialRepoSectionState =
        let
          val name = NONE
          and url = NONE
        in
          RepoSectionState
            {name = name,
             url = url}
        end;

    fun addNameRepoSectionState n state =
        let
          val RepoSectionState {name,url} = state

          val name =
              case name of
                NONE => SOME n
              | SOME n' =>
                raise Error ("duplicate \"" ^ nameRepoSectionKey ^
                             "\" keys: " ^ n ^ " and " ^ n')
        in
          RepoSectionState
            {name = name,
             url = url}
        end;

    fun addUrlRepoSectionState u state =
        let
          val RepoSectionState {name,url} = state

          val url =
              case url of
                NONE => SOME u
              | SOME u' =>
                raise Error ("duplicate \"" ^ urlRepoSectionKey ^
                             "\" keys: " ^ u ^ " and " ^ u')
        in
          RepoSectionState
            {name = name,
             url = url}
        end;

    fun processRepoSectionState (kv,state) =
        let
          val Config.KeyValue {key,value} = kv
        in
          if key = nameRepoSectionKey then
            addNameRepoSectionState value state
          else if key = urlRepoSectionKey then
            addUrlRepoSectionState value state
          else
            raise Error ("unknown key \"" ^ key ^ "\"")
        end;

    fun finalRepoSectionState state =
        let
          val RepoSectionState {name,url} = state

          val name =
              case name of
                SOME n => n
              | NONE =>
                raise Error ("missing \"" ^ nameRepoSectionKey ^ "\" key")

          val url =
              case url of
                SOME u => u
              | NONE =>
                raise Error ("missing \"" ^ urlRepoSectionKey ^ "\" key")
        in
          mkRepo
            {name = name,
             url = url}
        end;
  in
    fun fromRepoSection kvs =
        let
          val state = initialRepoSectionState

          val state = List.foldl processRepoSectionState state kvs
        in
          finalRepoSectionState state
        end
        handle Error err =>
          raise Error ("in \"" ^ repoConfigSection ^
                       "\" section of config file:\n" ^ err);
  end;

  fun addSection (sect,conf) =
      let
        val Config.Section {name,keyValues} = sect
      in
        if name = repoConfigSection then
          let
            val r = fromRepoSection keyValues
          in
            addRepoConfig conf r
          end
        else
          raise Error ("unknown config section \"" ^ name ^ "\"")
      end;

  fun fromSections conf =
      let
        val Config.Config {sections} = conf
      in
        List.foldl addSection emptyConfig sections
      end;
in
  fun readConfig filename =
      let
        val conf = Config.fromTextFile filename handle IO.Io _ => Config.empty
      in
        fromSections conf
      end
(*OpenTheoryDebug
      handle Error err => raise Bug ("Directory.readConfig: " ^ err);
*)
end;

local
  fun toRepoSection repo =
      let
        val n = nameRepo repo
        and u = urlRepo repo
      in
        Config.Section
          {name = repoConfigSection,
           keyValues =
             [Config.KeyValue
                {key = nameRepoSectionKey,
                 value = n},
              Config.KeyValue
                {key = urlRepoSectionKey,
                 value = u}]}
      end;

  fun toSections config =
      let
        val Config {repos = rs} = config

        val sections = map toRepoSection rs
      in
        Config.Config {sections = sections}
      end;
in
  val ppConfig = Print.ppMap toSections Config.pp;
end;

fun writeConfig {config,filename} =
    let
      val s = Print.toStream ppConfig config
    in
      Stream.toTextFile {filename = filename} s
    end;

val defaultConfig =
    let
      val repos = [openTheoryRepo]
    in
      Config {repos = repos}
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

val pp = Print.ppMap root (Print.ppBracket "<" ">" ppDirectory);

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

(* ------------------------------------------------------------------------- *)
(* Listing packages in the package directory.                                *)
(* ------------------------------------------------------------------------- *)

local
  fun addName (s,pkgs) =
      PackageNameSet.add pkgs (PackageName.fromString s);
in
  fun list dir =
      let
        val Directory {rootDirectory = root, ...} = dir

        val pkgDir = mkPackagesDirectory {rootDirectory = root}

        val dirStrm = OS.FileSys.openDir pkgDir

        fun readAll acc =
            case OS.FileSys.readDir dirStrm of
              NONE => acc
            | SOME s => readAll (s :: acc)

        val pkgs = readAll []

        val () = OS.FileSys.closeDir dirStrm
      in
        List.foldl addName PackageNameSet.empty pkgs
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Installing new packages into the package directory.                       *)
(* ------------------------------------------------------------------------- *)

fun install dir {filename} =
    raise Bug "Directory.install: not implemented";

(* ------------------------------------------------------------------------- *)
(* Uploading packages from the package directory to a repo.                  *)
(* ------------------------------------------------------------------------- *)

fun upload dir repo pkg =
    raise Bug "Directory.upload: not implemented";

(* ------------------------------------------------------------------------- *)
(* Downloading packages from a repo to the package directory.                *)
(* ------------------------------------------------------------------------- *)

fun download dir repo pkg =
    raise Bug "Directory.download: not implemented";

end
