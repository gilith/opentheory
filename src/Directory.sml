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

fun renameDirectory {src,dest} = OS.FileSys.rename {old = src, new = dest};

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

(* ------------------------------------------------------------------------- *)
(* The package staging directory.                                            *)
(* ------------------------------------------------------------------------- *)

fun mkStagingDirectory {rootDirectory = dir} =
    OS.Path.joinDirFile {dir = dir, file = stagingDirectory};

fun mkStagingPackageDirectory root name =
    let
      val dir = mkStagingDirectory root
      and file = PackageName.toString name
    in
      OS.Path.joinDirFile {dir = dir, file = file}
    end;

local
  fun warnFile {filename} =
      warn ("activity in staging area: " ^ filename);
in
  fun checkStagingDirectory dir =
      let
        val files = readDirectory dir
      in
        List.app warnFile files
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Article filenames.                                                        *)
(* ------------------------------------------------------------------------- *)

fun isArticleFile {filename} =
    case OS.Path.ext (OS.Path.file filename) of
      SOME ext => ext = articleFileExtension
    | NONE => false;

fun normalizeArticleFile {filename} =
    let
      val filename =
          OS.Path.joinBaseExt
            {base = OS.Path.base (OS.Path.file filename),
             ext = SOME articleFileExtension}
    in
      {filename = filename}
    end;

(* ------------------------------------------------------------------------- *)
(* A type of directory operation errors.                                     *)
(* ------------------------------------------------------------------------- *)

datatype error =
    AlreadyInstalledError
  | FilenameClashError of
      {srcs : {name : string, filename : string option} list,
       dest : {filename : string}}
  | InstalledDescendentError of PackageName.name
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
    | FilenameClashError _ => true
    | InstalledDescendentError _ => true
    | NotInstalledError => true
    | UninstalledParentError _ => true;

fun toStringError err =
    (if isFatalError err then "Error" else "Warning") ^ ": " ^
    (case err of
       AlreadyInstalledError =>
       "package is already installed"
     | FilenameClashError {srcs,dest} =>
       let
         fun toStringSrc {name,filename} =
             name ^
             (case filename of
                SOME sf => ": " ^ PackageTheory.toStringFilename {filename = sf}
              | NONE => "")
       in
         "filename clash in package directory:\n" ^
         "Package file " ^ PackageTheory.toStringFilename dest ^ "\n" ^
         " is target for " ^ join "\n  and also for " (map toStringSrc srcs)
       end
     | InstalledDescendentError name =>
       "in use by installed package: " ^ PackageName.toString name
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
      fun add acc set = PackageNameSet.foldl check acc set

      and check (name,acc) =
          if PackageNameSet.member name acc then acc
          else expand (PackageNameSet.add acc name) name

      and expand acc name = add acc (f name)
    in
      add PackageNameSet.empty
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

fun ancestorsPackageDeps deps =
    let
      val step = parentsPackageDeps deps
    in
      fn name => closePackageSet step (step name)
    end;

fun descendentsPackageDeps deps =
    let
      val step = childrenPackageDeps deps
    in
      fn name => closePackageSet step (step name)
    end;

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

      val () =
          let
            val directory = mkStagingDirectory {rootDirectory = rootDirectory}

            val () = checkStagingDirectory {directory = directory}
          in
            ()
          end
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

fun stagingPackageDirectory dir name =
    let
      val Directory {rootDirectory = root, ...} = dir
    in
      mkStagingPackageDirectory {rootDirectory = root} name
    end;

fun stagingPackageInfo dir name =
    let
      val directory = stagingPackageDirectory dir name
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

fun ancestors dir =
    let
      val step = parents dir
    in
      fn name => closePackageSet step (step name)
    end;

fun ancestorsSet dir names = closePackageSet (parents dir) names;

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
(* Staging theory files for installation.                                    *)
(* ------------------------------------------------------------------------- *)

fun normalizeExtraFile {filename} =
    {filename = OS.Path.file filename};

local
  fun checkDep dir (name,errs) =
      if installed dir name then errs
      else UninstalledParentError name :: errs;

  fun mkFileCopyPlan info pkg =
      let
        fun add (src, {filename = dest}) plan =
            let
              val hits = Option.getOpt (StringMap.peek plan dest, [])

              val hits = src :: hits

              val plan = StringMap.insert plan (dest,hits)
            in
              plan
            end

        fun addReserved ((name,filename),plan) =
            let
              val src = {name = name, filename = NONE}
              and dest = filename
            in
              add (src,dest) plan
            end

        fun addArticle ({filename},plan) =
            let
              val src = {name = "article file", filename = SOME filename}

              val dest = normalizeArticleFile {filename = filename}
            in
              add (src,dest) plan
            end

        fun addExtra ({name,filename},plan) =
            let
              val src = {name = name, filename = SOME filename}

              val dest = normalizeExtraFile {filename = filename}
            in
              add (src,dest) plan
            end

        val reserved =
            [("theory file", PackageInfo.theoryFile info)]

        val plan = StringMap.new ()

        val plan = List.foldl addReserved plan reserved

        val plan = List.foldl addArticle plan (Package.articles pkg)

        val plan = List.foldl addExtra plan (Package.extraFiles pkg)
      in
        plan
      end;

(*OpenTheoryDebug
  val ppFileCopyPlan =
      let
        fun ppSrc {name,filename} =
            Print.program
              (Print.ppString name ::
               (case filename of
                  NONE => []
                | SOME sf => [Print.addString " ", Print.ppString sf]))

        fun ppCopy (dest,srcs) =
            Print.blockProgram Print.Consistent 2
              (Print.ppString dest ::
               Print.ppString ":" ::
               map (Print.sequence Print.addNewline o ppSrc) srcs)
      in
        fn plan =>
           case StringMap.toList plan of
             [] => Print.skip
           | cp :: cps =>
             Print.blockProgram Print.Consistent 0
               (ppCopy cp ::
                map (Print.sequence Print.addNewline o ppCopy) cps)
      end;
*)

  val checkFileCopyPlan =
      let
        fun check (dest,srcs,errs) =
            if length srcs <= 1 then errs
            else
              let
                val dest = {filename = dest}
              in
                FilenameClashError {srcs = srcs, dest = dest} :: errs
              end
      in
        StringMap.foldl check
      end;
in
  fun checkStageTheory dir name pkg =
      let
        val errs = []

        val errs =
            if not (installed dir name) then errs
            else AlreadyInstalledError :: errs

        val errs = List.foldl (checkDep dir) errs (Package.packages pkg)

        val info = packageInfo dir name

        val plan = mkFileCopyPlan info pkg

(*OpenTheoryTrace1
        val () =
            Print.trace ppFileCopyPlan "Directory.checkStageTheory: plan" plan
*)

        val errs = checkFileCopyPlan errs plan
      in
        rev errs
      end;
end;

local
  fun copyArticle srcDir info thy =
      let
        val PackageTheory.Theory {name,imports,node} = thy
      in
        case node of
          PackageTheory.Article
            {interpretation = int,
             filename = filename} =>
          let
            val srcFilename = OS.Path.concat (srcDir,filename)

            val art =
                Article.fromTextFile
                  {savable = true,
                   import = Article.empty,
                   interpretation = Interpretation.natural,
                   filename = srcFilename}

            val {filename = pkgFilename} =
                normalizeArticleFile {filename = filename}

            val {filename = destFilename} =
                PackageInfo.joinDirectory info {filename = pkgFilename}

            val () =
                Article.toTextFile
                  {article = art,
                   filename = destFilename}

            val node =
                PackageTheory.Article
                  {interpretation = int,
                   filename = pkgFilename}
          in
            PackageTheory.Theory
              {name = name,
               imports = imports,
               node = node}
          end
        | _ => thy
      end;

  fun copyExtraFile srcDir info tag =
      case Package.destExtraFile tag of
        NONE => tag
      | SOME {name,filename} =>
        let
          val srcFilename = OS.Path.concat (srcDir,filename)

          val {filename = pkgFilename} =
              normalizeExtraFile {filename = filename}

          val {filename = destFilename} =
              PackageInfo.joinDirectory info {filename = pkgFilename}

          val cmd = "cp " ^ srcFilename ^ " " ^ destFilename

(*OpenTheoryTrace1
          val () = print (cmd ^ "\n")
*)

          val () =
              if OS.Process.isSuccess (OS.Process.system cmd) then ()
              else raise Error "copy failed"
        in
          Package.mkExtraFile {name = name, filename = pkgFilename}
        end;

  fun copyArticles srcDir info pkg =
      let
        val Package.Package' {tags,theories} = Package.dest pkg

        val theories = map (copyArticle srcDir info) theories
      in
        Package.mk (Package.Package' {tags = tags, theories = theories})
      end;

  fun copyExtraFiles srcDir info pkg =
      let
        val Package.Package' {tags,theories} = Package.dest pkg

        val tags = map (copyExtraFile srcDir info) tags
      in
        Package.mk (Package.Package' {tags = tags, theories = theories})
      end;
in
  fun stageTheory dir name pkg {directory = srcDir} =
      let
(*OpenTheoryDebug
        val errs = checkStageTheory dir name pkg

        val _ = not (List.exists isFatalError errs) orelse
                raise Bug "Directory.stageTheory: fatal error"
*)
        (* Make a package info for the stage directory *)

        val stageInfo = stagingPackageInfo dir name

        (* Create the stage directory *)

        val () = PackageInfo.createDirectory stageInfo
      in
        let
          (* Copy the articles over *)

          val pkg = copyArticles srcDir stageInfo pkg

          (* Copy the extra files over *)

          val pkg = copyExtraFiles srcDir stageInfo pkg

          (* Write the new theory file *)

          val () =
              let
                val file = PackageInfo.theoryFile stageInfo

                val {filename} = PackageInfo.joinDirectory stageInfo file
              in
                Package.toTextFile {package = pkg, filename = filename}
              end
        in
          ()
        end
        handle e =>
          let
            val () = PackageInfo.nukeDirectory stageInfo
          in
            raise e
          end
      end
end;

(* ------------------------------------------------------------------------- *)
(* Installing staged packages into the package directory.                    *)
(* ------------------------------------------------------------------------- *)

fun installStaged dir name =
    let
      val stageInfo = stagingPackageInfo dir name

      val pkgInfo = packageInfo dir name
    in
      let
        (* Rename staged package directory to package directory *)

        val {directory = stageDir} = PackageInfo.directory stageInfo

        val {directory = pkgDir} = PackageInfo.directory pkgInfo

        val () = renameDirectory {src = stageDir, dest = pkgDir}

        (* Update the list of installed packages *)

        val Directory {packages as ref pkgs, ...} = dir

        val () = packages := addPackages pkgs pkgInfo
      in
        ()
      end
      handle e =>
        let
          val () = PackageInfo.nukeDirectory stageInfo
        in
          raise e
        end
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
(* Make tarball ready for uploading.                                         *)
(* ------------------------------------------------------------------------- *)

fun tarball dir name = raise Bug "Directory.tarball";

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
