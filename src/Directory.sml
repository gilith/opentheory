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

val articleFileExtension = "art"
and configFile = "config"
and nameRepoSectionKey = "name"
and openTheoryRepoName = "gilith"
and openTheoryRepoUrl = "http://opentheory.gilith.com/"
and packagesDirectory = "packages"
and stagingDirectory = "staging"
and repoConfigSection = "repo"
and urlRepoSectionKey = "url";

(* ------------------------------------------------------------------------- *)
(* Directories and filenames.                                                *)
(* ------------------------------------------------------------------------- *)

fun createDirectory {directory} = OS.FileSys.mkDir directory;

fun ppDirectory {directory} = Print.ppString directory;

fun mkConfigFilename {rootDirectory = dir} =
    OS.Path.joinDirFile {dir = dir, file = configFile};

fun mkPackagesDirectory {rootDirectory = dir} =
    OS.Path.joinDirFile {dir = dir, file = packagesDirectory};

fun mkPackageDirectory root name =
    let
      val dir = mkPackagesDirectory root
      and file = PackageName.toString name
    in
      OS.Path.joinDirFile {dir = dir, file = file}
    end;

fun mkStagingDirectory {rootDirectory = dir} =
    OS.Path.joinDirFile {dir = dir, file = stagingDirectory};

fun mkStagingPackageDirectory root name =
    let
      val dir = mkStagingDirectory root
      and file = PackageName.toString name
    in
      OS.Path.joinDirFile {dir = dir, file = file}
    end;

(* ------------------------------------------------------------------------- *)
(* A type of directory operation errors.                                     *)
(* ------------------------------------------------------------------------- *)

datatype error =
    AlreadyInstalledError
  | ArticleExtensionError of {filename : string}
  | FilenameClashError of {filename : string} * string list
  | InstalledDescendentError of PackageName.name
  | NonemptyPathError of {filename : string}
  | NotInstalledError
  | UninstalledParentError of PackageName.name;

fun isAlreadyInstalledError err =
    case err of
      AlreadyInstalledError => true
    | _ => false;

fun removeAlreadyInstalledError errs =
    let
      val (xs,errs) = List.partition isAlreadyInstalledError errs

      val removed = not (null xs)
    in
      (removed,errs)
    end;

fun destInstalledDescendentError err =
    case err of
      InstalledDescendentError name => SOME name
    | _ => NONE;

fun isInstalledDescendentError err =
    Option.isSome (destInstalledDescendentError err);

val removeInstalledDescendentError =
    let
      fun remove (err,(names,errs)) =
          case destInstalledDescendentError err of
            SOME name => (name :: names, errs)
          | NONE => (names, err :: errs)
    in
      List.foldr remove ([],[])
    end;

fun isFatalError err =
    case err of
      AlreadyInstalledError => true
    | ArticleExtensionError _ => false
    | FilenameClashError _ => true
    | InstalledDescendentError _ => true
    | NonemptyPathError _ => false
    | NotInstalledError => true
    | UninstalledParentError _ => true;

fun toStringError err =
    (if isFatalError err then "Error" else "Warning") ^ ": " ^
    (case err of
       AlreadyInstalledError =>
       "package is already installed"
     | ArticleExtensionError {filename} =>
       "strange article filename extension: " ^ filename
     | FilenameClashError ({filename},fl) =>
       "filename clash in package directory: " ^ filename ^
       String.concat (map (fn f => "\n  " ^ f) fl)
     | InstalledDescendentError name =>
       "in use by installed package: " ^ PackageName.toString name
     | NonemptyPathError {filename} =>
       "file not in package directory: " ^ filename
     | NotInstalledError =>
       "package is not installed"
     | UninstalledParentError name =>
       "depends on uninstalled package: " ^ PackageName.toString name);

fun toStringErrorList errs = join "\n" (map toStringError errs);

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
(* A type of theory packages.                                                *)
(* ------------------------------------------------------------------------- *)

datatype packages =
    Packages of PackageInfo.info PackageNameMap.map;

val emptyPackages = Packages (PackageNameMap.new ());

fun peekPackages (Packages pkgs) name = PackageNameMap.peek pkgs name;

fun knownPackages (Packages pkgs) name = PackageNameMap.inDomain name pkgs;

fun unknownPackages pkgs name = not (knownPackages pkgs name);

local
  fun addName (name,_,acc) = PackageNameSet.add acc name;
in
  fun listPackages (Packages pkgs) =
      PackageNameMap.foldl addName PackageNameSet.empty pkgs;
end;

fun foldlPackages f =
    let
      fun f' (_,info,acc) = f (info,acc)
    in
      fn acc => fn Packages pkgs => PackageNameMap.foldl f' acc pkgs
    end;

fun addPackages (Packages pkgs) info =
    let
      val name = PackageInfo.name info

      val pkgs = PackageNameMap.insert pkgs (name,info)
    in
      Packages pkgs
    end;

fun deletePackages (Packages pkgs) name =
    let
      val pkgs = PackageNameMap.delete pkgs name
    in
      Packages pkgs
    end;

fun removePackages (Packages pkgs) name =
    let
      val pkgs = PackageNameMap.remove pkgs name
    in
      Packages pkgs
    end;

local
  fun addDir ({filename},pkgs) =
      let
        val name = PackageName.fromString (OS.Path.file filename)

        val info = PackageInfo.mk {name = name, directory = filename}
      in
        addPackages pkgs info
      end;
in
  fun readPackages dir =
      let
        val dirs = readDirectory dir
      in
        List.foldl addDir emptyPackages dirs
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Package dependency operations.                                            *)
(* ------------------------------------------------------------------------- *)

fun closePackageSet f =
    let
      fun step acc name = PackageNameSet.foldl add acc (f name)

      and add (name,acc) =
          if PackageNameSet.member name acc then acc
          else step (PackageNameSet.add acc name) name
    in
      step PackageNameSet.empty
    end;

fun sortPackageSet f s =
    let
      fun dfsCheck (name,(seen,acc)) =
          if PackageNameSet.member name seen then (seen,acc)
          else dfsName (seen,acc) name

      and dfsName (seen,acc) name =
          let
            val seen = PackageNameSet.add seen name

            val (seen,acc) = dfsSet (seen,acc) (f name)

            val acc = if PackageNameSet.member name s then name :: acc else acc
          in
            (seen,acc)
          end

      and dfsSet seen_acc names = PackageNameSet.foldl dfsCheck seen_acc names

      val (_,acc) = dfsSet (PackageNameSet.empty,[]) s
    in
      rev acc
    end;

(* ------------------------------------------------------------------------- *)
(* Package dependency graphs.                                                *)
(* ------------------------------------------------------------------------- *)

datatype packageDeps =
    PackageDeps of
      {parents : PackageNameSet.set PackageNameMap.map,
       children : PackageNameSet.set PackageNameMap.map};

val emptyPackageDeps =
    let
      val parents = PackageNameMap.new ()
      and children = PackageNameMap.new ()
    in
      PackageDeps
        {parents = parents,
         children = children}
    end;

fun parentsPackageDeps (PackageDeps {parents,...}) name =
    case PackageNameMap.peek parents name of
      SOME ps => ps
    | NONE => PackageNameSet.empty;

fun childrenPackageDeps (PackageDeps {children,...}) name =
    case PackageNameMap.peek children name of
      SOME cs => cs
    | NONE => PackageNameSet.empty;

fun ancestorsPackageDeps deps = closePackageSet (parentsPackageDeps deps);

fun descendentsPackageDeps deps = closePackageSet (childrenPackageDeps deps);

fun addPackageDeps deps (p,c) =
    let
      val ps = parentsPackageDeps deps c
      and cs = childrenPackageDeps deps p

      val PackageDeps {parents,children} = deps

      val parents =
          if PackageNameSet.member p ps then parents
          else PackageNameMap.insert parents (c, PackageNameSet.add ps p)

      val children =
          if PackageNameSet.member c cs then children
          else PackageNameMap.insert children (p, PackageNameSet.add cs c)
    in
      PackageDeps
        {parents = parents,
         children = children}
    end;

fun addInfoPackageDeps deps info =
    let
      val name = PackageInfo.name info
      and pars = PackageInfo.packages info
    in
      List.foldl (fn (p,d) => addPackageDeps d (p,name)) deps pars
    end;

val fromPackagesPackageDeps =
    let
      fun add (info,deps) = addInfoPackageDeps deps info
    in
      foldlPackages add emptyPackageDeps
    end;

fun sortPackageDeps deps = sortPackageSet (parentsPackageDeps deps);

(* ------------------------------------------------------------------------- *)
(* A type of theory package directories.                                     *)
(* ------------------------------------------------------------------------- *)

datatype directory =
    Directory of
      {rootDirectory : string,
       config : config,
       packages : packages ref};

fun create {rootDirectory} =
    let
      val () =
          let
            val dir = rootDirectory
          in
            createDirectory {directory = dir}
          end

      val () =
          let
            val dir = mkPackagesDirectory {rootDirectory = rootDirectory}
          in
            createDirectory {directory = dir}
          end

      val () =
          let
            val dir = mkStagingDirectory {rootDirectory = rootDirectory}
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

      val packages = ref emptyPackages
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

      val packages =
          let
            val directory = mkPackagesDirectory {rootDirectory = rootDirectory}
          in
            ref (readPackages {directory = directory})
          end

(***
      val () = warn "FIXME: clean up staging directory"
***)
    in
      Directory
        {rootDirectory = rootDirectory,
         config = config,
         packages = packages}
    end;

fun root (Directory {rootDirectory = x, ...}) = {directory = x};

fun config (Directory {config = x, ...}) = x;

fun packages (Directory {packages = ref x, ...}) = x;

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

fun packageInfo dir name =
    let
      val directory = packageDirectory dir name
    in
      PackageInfo.mk {name = name, directory = directory}
    end;

val pp = Print.ppMap root (Print.ppBracket "<" ">" ppDirectory);

(* ------------------------------------------------------------------------- *)
(* Looking up packages in the package directory.                             *)
(* ------------------------------------------------------------------------- *)

fun peek dir name = peekPackages (packages dir) name;

fun get dir name =
    case peek dir name of
      SOME info => info
    | NONE => raise Error "Directory.get";

fun installed dir name = Option.isSome (peek dir name);

(* ------------------------------------------------------------------------- *)
(* Dependencies in the package directory.                                    *)
(* ------------------------------------------------------------------------- *)

(* Operations that do not need to create a package dependency graph *)

fun parents dir name =
    case peek dir name of
      SOME info => PackageNameSet.fromList (PackageInfo.packages info)
    | NONE => raise Bug "Directory.parents";

fun sortByAge dir set = sortPackageSet (parents dir) set;

fun ancestors dir = closePackageSet (parents dir);

fun ancestorsByAge dir name = sortByAge dir (ancestors dir name);

(* Operations that use a package dependency graph *)

fun packageDeps dir = fromPackagesPackageDeps (packages dir);

fun children dir name =
    let
(*OpenTheoryDebug
      val _ = installed dir name orelse
              raise Bug "Directory.children: unknown"
*)
      val deps = packageDeps dir
    in
      childrenPackageDeps deps name
    end;

fun descendents dir name =
    let
(*OpenTheoryDebug
      val _ = installed dir name orelse
              raise Bug "Directory.descendents: unknown"
*)
      val deps = packageDeps dir
    in
      descendentsPackageDeps deps name
    end;

fun descendentsByAge dir name =
    let
(*OpenTheoryDebug
      val _ = installed dir name orelse
              raise Bug "Directory.descendents: unknown"
*)
      val deps = packageDeps dir

      val desc = descendentsPackageDeps deps name
    in
      sortPackageDeps deps desc
    end;

(* ------------------------------------------------------------------------- *)
(* Nuke a theory package (be warned: this is not very polite).               *)
(* ------------------------------------------------------------------------- *)

fun nuke dir name =
    let
      val Directory {packages as ref pkgs, ...} = dir

      val info = packageInfo dir name

      val () = PackageInfo.nukeDirectory info

      val () = packages := removePackages pkgs name
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Listing packages in the package directory.                                *)
(* ------------------------------------------------------------------------- *)

fun list dir =
    let
      val Directory {packages = ref pkgs, ...} = dir
    in
      listPackages pkgs
    end;

fun listByAge dir = sortPackageDeps (packageDeps dir) (list dir);

(* ------------------------------------------------------------------------- *)
(* Installing packages into the package directory.                           *)
(* ------------------------------------------------------------------------- *)

local
  fun checkDep dir (name,errs) =
      if installed dir name then errs
      else UninstalledParentError name :: errs;

  fun normalizeArticle {filename} =
      OS.Path.joinBaseExt
        {base = OS.Path.base (OS.Path.file filename),
         ext = SOME articleFileExtension};

  val planCopyInstall =
      let
        fun add ((src,dest),acc) =
            let
              val file =

              val filenames = Option.getOpt (StringMap.peek acc file, [])

              val filenames = {filename = filename} :: filenames

              val acc = StringMap.insert acc (file,filenames)
            in
              acc
            end

        fun addArticle ({filename},acc) =
            
      in
        List.foldl add (StringMap.new ())
      end;

  fun checkPath ({filename},errs) =
      let
        val dir = OS.Path.dir filename
      in
        if dir = "" then errs
        else NonemptyPathError {filename = filename} :: errs
      end;

  fun checkCopyInstall (file,filenames,errs) =
      let
        val errs =
            if length filenames = 1 then errs
            else FilenameClashError filenames :: errs

        val errs = List.foldl checkPath errs filenames
      in
        errs
      end;
in
  fun checkStageTheory dir name pkg =
      let
        val errs = []

        val errs =
            if not (installed dir name) then errs
            else AlreadyInstalledError :: errs

        val errs = List.foldl (checkDep dir) errs (Package.packages pkg)

        val plan = planCopyInstall (Package.files pkg)

        val errs = StringMap.foldl checkCopyInstall errs plan
      in
        rev errs
      end;
end;

local
  fun copy_art src_dir info thy =
      let
        val PackageTheory.Theory {name,imports,node} = thy
      in
        case node of
          PackageTheory.Article
            {interpretation = int,
             filename = src_filename} =>
          let
            val pkg_file = OS.Path.file src_filename

            val src_filename = OS.Path.concat (src_dir,src_filename)

            val art =
                Article.fromTextFile
                  {savable = true,
                   import = Article.empty,
                   interpretation = Interpretation.natural,
                   filename = src_filename}

            val {filename = dest_filename} =
                PackageInfo.joinDirectory info {filename = pkg_file}

            val () =
                Article.toTextFile
                  {article = art,
                   filename = dest_filename}

            val node =
                PackageTheory.Article
                  {interpretation = int,
                   filename = pkg_file}
          in
            PackageTheory.Theory
              {name = name,
               imports = imports,
               node = node}
          end
        | _ => thy
      end;
in
  fun stageTheory dir name pkg {filename = src_filename} =
      let
(*OpenTheoryDebug
        val errs = checkStageTheory dir name pkg

        val _ = not (List.exists isFatalError errs) orelse
                raise Bug "Directory.stageTheory: fatal error"
*)

        val info = packageInfo dir name

        val src_dir = OS.Path.dir src_filename

        (* Make the package directory *)

        val () = PackageInfo.createDirectory info

        (* Copy the articles over *)

        val Package.Package' {tags,theories} = Package.dest pkg

        val theories = map (copy_art src_dir info) theories

        val pkg =
            Package.mk (Package.Package' {tags = tags, theories = theories})

        (* Write the new package file *)

        val () =
            let
              val {filename} = PackageInfo.packageFile info
            in
              Package.toTextFile {package = pkg, filename = filename}
            end
      in
        ()
      end;
end;

fun installStage dir name =
    let
      (*** Rename stage to package dir ***)

      (* Lastly, update the list of installed packages *)

      val Directory {packages as ref pkgs, ...} = dir

      val () = packages := addPackages pkgs info
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Uninstalling packages from the package directory.                         *)
(* ------------------------------------------------------------------------- *)

fun checkUninstall dir name =
    if not (installed dir name) then [NotInstalledError]
    else
      let
        val errs = []

        val desc = descendents dir name

        val errs =
            if PackageNameSet.null desc then errs
            else
              let
                fun add (n,acc) = InstalledDescendentError n :: acc
              in
                PackageNameSet.foldl add errs desc
              end
      in
        rev errs
      end;

fun uninstall dir name =
    let
(*OpenTheoryDebug
        val _ = not (List.exists isFatalError (checkUninstall dir name)) orelse
                raise Bug "Directory.uninstall: fatal error"
*)

      (* Nuke the theory package *)

      val () = nuke dir name
    in
      ()
    end;

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

(* ------------------------------------------------------------------------- *)
(* A package finder.                                                         *)
(* ------------------------------------------------------------------------- *)

fun finder dir = PackageFinder.mk (peek dir);

end
