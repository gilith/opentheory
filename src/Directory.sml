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
and urlRepoSectionKey = "url";

(* ------------------------------------------------------------------------- *)
(* Directories and filenames.                                                *)
(* ------------------------------------------------------------------------- *)

fun createDirectory {directory} = OS.FileSys.mkDir directory;

fun filesDirectory {directory} =
    let
      val dirStrmOpt =
          SOME (OS.FileSys.openDir directory)
          handle IO.Io _ => NONE
    in
      case dirStrmOpt of
        NONE => NONE
      | SOME dirStrm =>
        let
          fun readAll acc =
              case OS.FileSys.readDir dirStrm of
                NONE => acc
              | SOME f => readAll (StringSet.add acc f)

          val files = readAll StringSet.empty

          val () = OS.FileSys.closeDir dirStrm
        in
          SOME files
        end
    end;

fun existsDirectory directory = Option.isSome (filesDirectory directory);

fun nukeDirectory {directory} =
    case filesDirectory {directory = directory} of
      NONE => ()
    | SOME files =>
      let
        fun nuke file =
            let
              val filename =
                  OS.Path.joinDirFile
                    {dir = directory, file = file}
            in
              OS.FileSys.remove filename
            end

        val () = StringSet.app nuke files

        val () = OS.FileSys.rmDir directory
      in
        ()
      end;

fun ppDirectory {directory} = Print.ppString directory;

fun mkConfigFilename {rootDirectory} =
    OS.Path.joinDirFile {dir = rootDirectory, file = configFile};

fun mkPackagesDirectory {rootDirectory} =
    OS.Path.joinDirFile
      {dir = rootDirectory, file = packagesDirectory};

fun mkPackageDirectory root name =
    let
      val directory = mkPackagesDirectory root
    in
      OS.Path.joinDirFile
        {dir = directory, file = PackageName.toString name}
    end;

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
       packages : PackageInfo.info option PackageNameMap.map ref};

fun newPackages () = ref (PackageNameMap.new ());

fun create {rootDirectory} =
    let
      val () = createDirectory {directory = rootDirectory}

      val () =
          let
            val dir = mkPackagesDirectory {rootDirectory = rootDirectory}
          in
            createDirectory {directory = dir}
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

fun packagesDirectory dir =
    let
      val Directory {rootDirectory = root, ...} = dir
    in
      mkPackagesDirectory {rootDirectory = root}
    end;

fun packageDirectory dir name =
    let
      val Directory {rootDirectory = root, ...} = dir
    in
      mkPackageDirectory {rootDirectory = root} name
    end;

val pp = Print.ppMap root (Print.ppBracket "<" ">" ppDirectory);

(* ------------------------------------------------------------------------- *)
(* Looking up packages in the package directory.                             *)
(* ------------------------------------------------------------------------- *)

local
  fun lookupPackage dir name =
      let
        val directory = packageDirectory dir name

        val info = PackageInfo.mk {name = name, directory = directory}
      in
        if PackageInfo.installed info then SOME info else NONE
      end;

  fun lookupCached dir name =
      let
        val Directory {packages as ref pkgs, ...} = dir
      in
        case PackageNameMap.peek pkgs name of
          SOME pkg => pkg
        | NONE =>
          let
            val pkg = lookupPackage dir name

            val pkgs = PackageNameMap.insert pkgs (name,pkg)

            val () = packages := pkgs
          in
            pkg
          end
      end;
in
  fun lookup dir = PackageFinder.mk (lookupCached dir);

  fun installed dir name = Option.isSome (lookupCached dir name);
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
        val pkgDir = packagesDirectory dir
      in
        case filesDirectory {directory = pkgDir} of
          SOME pkgs => StringSet.foldl addName PackageNameSet.empty pkgs
        | NONE => raise Error "couldn't read packages directory"
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Uninstall a package.                                                      *)
(* ------------------------------------------------------------------------- *)

fun nukePackageDirectory dir name =
    let
      val directory = packageDirectory dir name

      val () = nukeDirectory {directory = directory}
    in
      ()
    end;

fun nuke dir name =
    let
      val () = nukePackageDirectory dir name
    in
      ()
    end;

fun uninstall dir name =
    let
(*OpenTheoryDebug
      val () = warn "Directory.uninstall: check dependencies before nuking"
*)
    in
      nuke dir name
    end;

(* ------------------------------------------------------------------------- *)
(* Check whether it is possible to install a new package.                    *)
(* ------------------------------------------------------------------------- *)

datatype errorInstall =
    DirectoryExistsInstall
  | UninstalledDependencyInstall of PackageName.name
  | NonemptyPathInstall of {filename : string}
  | NameClashInstall of {filename : string} list;

fun isDirectoryExistsInstall err =
    case err of
      DirectoryExistsInstall => true
    | _ => false;

fun removeDirectoryExistsInstall errs =
    let
      val (des,errs) = List.partition isDirectoryExistsInstall errs

      val replace = not (null des)
    in
      (replace,errs)
    end;

fun fatalErrorInstall err =
    case err of
      DirectoryExistsInstall => true
    | UninstalledDependencyInstall _ => true
    | NonemptyPathInstall _ => false
    | NameClashInstall _ => true;

fun toStringErrorInstall err =
    (if fatalErrorInstall err then "Error" else "Warning") ^ ": " ^
    (case err of
       DirectoryExistsInstall =>
       "package directory already exists"
     | UninstalledDependencyInstall name =>
       "uninstalled package dependency: " ^ PackageName.toString name
     | NonemptyPathInstall {filename} =>
       "file not in package directory: " ^ filename
     | NameClashInstall fl =>
       "filename clash:" ^
       String.concat (map (fn {filename = f} => "\n  " ^ f) fl));

fun toStringErrorInstallList errs =
    join "\n" (map toStringErrorInstall errs);

local
  fun checkDep dir (name,errs) =
      if installed dir name then errs
      else UninstalledDependencyInstall name :: errs;

  val planCopyInstall =
      let
        fun add ({filename},acc) =
            let
              val file = OS.Path.file filename

              val filenames = Option.getOpt (StringMap.peek acc file, [])

              val filenames = {filename = filename} :: filenames

              val acc = StringMap.insert acc (file,filenames)
            in
              acc
            end
      in
        List.foldl add (StringMap.new ())
      end;

  fun checkPath ({filename},errs) =
      let
        val dir = OS.Path.dir filename
      in
        if dir = "" then errs
        else NonemptyPathInstall {filename = filename} :: errs
      end;

  fun checkCopyInstall (file,filenames,errs) =
      let
        val errs =
            if length filenames = 1 then errs
            else NameClashInstall filenames :: errs

        val errs = List.foldl checkPath errs filenames
      in
        errs
      end;
in
  fun checkInstall dir name pkg =
      let
        val errs = []

        val directory = packageDirectory dir name

        val errs =
            if not (existsDirectory {directory = directory}) then errs
            else DirectoryExistsInstall :: errs

        val errs = List.foldl (checkDep dir) errs (Package.packages pkg)

        val plan = planCopyInstall (Package.files pkg)

        val errs = StringMap.foldl checkCopyInstall errs plan
      in
        rev errs
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Installing new packages into the package directory.                       *)
(* ------------------------------------------------------------------------- *)

fun install dir name pkg {filename = src_filename} =
    let
      val info =
          let
            val directory = packageDirectory dir name
          in
            PackageInfo.mk {name = name, directory = directory}
          end

      val src_dir = OS.Path.dir src_filename

      fun copy_art thy =
          let
            val PackageTheory.Theory {name,imports,node} = thy
          in
            case node of
              PackageTheory.Article
                {interpretation = int,
                 filename = src_art} =>
              let
                val pkg_art = OS.Path.file src_art

                val src_art = OS.Path.concat (src_dir,src_art)

                val art =
                    Article.fromTextFile
                      {savable = true,
                       import = Article.empty,
                       interpretation = Interpretation.natural,
                       filename = src_art}

                val {filename = dest_art} =
                    PackageInfo.joinDirectory info {filename = pkg_art}

                val () =
                    Article.toTextFile
                      {article = art,
                       filename = dest_art}

                val node =
                    PackageTheory.Article
                      {interpretation = int,
                       filename = pkg_art}
              in
                PackageTheory.Theory
                  {name = name,
                   imports = imports,
                   node = node}
              end
            | _ => thy
          end

      (* Make the package directory *)

      val () = createDirectory (PackageInfo.directory info)

      (* Copy the articles over *)

      val Package.Package' {tags,theories} = Package.dest pkg

      val theories = map copy_art theories

      val pkg = Package.mk (Package.Package' {tags = tags, theories = theories})

      (* Lastly, write the new package file *)

      val () =
          let
            val {filename} = PackageInfo.theoryFile info
          in
            Package.toTextFile {package = pkg, filename = filename}
          end
    in
      ()
    end
    handle Error err => (nuke dir name; raise Error err);

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
