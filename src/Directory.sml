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
and installedName = "installed"
and packagesDirectory = "packages"
and stagingDirectory = "staging"
and reposDirectory = "repos";

(* ------------------------------------------------------------------------- *)
(* Directory operations.                                                     *)
(* ------------------------------------------------------------------------- *)

fun createDirectory {directory} = OS.FileSys.mkDir directory;

fun renameDirectory {src,dest} = OS.FileSys.rename {old = src, new = dest};

fun ppDirectory {directory} = Print.ppString directory;

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

fun mkInstalledFilename {rootDirectory = dir} =
    let
      val {filename = file} = DirectoryChecksums.mkFilename installedName

      val filename = OS.Path.joinDirFile {dir = dir, file = file}
    in
      {filename = filename}
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

local
  fun warnFile {filename} =
      warn ("activity in staging area: " ^ filename);
in
  fun checkStagingPackagesDirectory dir =
      let
        val files = readDirectory dir
      in
        List.app warnFile files
      end;
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

val existsFatalError = List.exists isFatalError;

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
(* A type of theory package directories.                                     *)
(* ------------------------------------------------------------------------- *)

datatype directory =
    Directory of
      {rootDirectory : string,
       config : DirectoryConfig.config,
       packages : DirectoryPackages.packages,
       repos : DirectoryRepo.repo list};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk {rootDirectory = rootDir} =
    let
      val config =
          let
            val filename = mkConfigFilename {rootDirectory = rootDir}
          in
            DirectoryConfig.fromTextFile filename
          end

      val packages =
          let
            val {directory = dir} =
                mkPackagesDirectory {rootDirectory = rootDir}

            val {filename = file} =
                mkInstalledFilename {rootDirectory = rootDir}
          in
            DirectoryPackages.mk {directory = dir, filename = file}
          end

      val () =
          let
            val dir = mkStagingPackagesDirectory {rootDirectory = rootDir}

            val () = checkStagingPackagesDirectory dir
          in
            ()
          end

      val repos =
          let
            fun mkRepo cfg =
                let
                  val {name} = DirectoryConfig.nameRepo cfg
                  and {url} = DirectoryConfig.urlRepo cfg

                  val {filename = file} =
                      mkRepoFilename {rootDirectory = rootDir} name
                in
                  DirectoryRepo.mk {name = name, url = url, filename = file}
                end

            val cfgs = DirectoryConfig.repos config
          in
            List.map mkRepo cfgs
          end
    in
      Directory
        {rootDirectory = rootDir,
         config = config,
         packages = packages,
         repos = repos}
    end;

fun root (Directory {rootDirectory = x, ...}) = {directory = x};

fun config (Directory {config = x, ...}) = x;

fun packages (Directory {packages = x, ...}) = x;

fun repos (Directory {repos = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Creating a new theory package directory.                                  *)
(* ------------------------------------------------------------------------- *)

fun create {rootDir} =
    let
      val () = createDirectory {directory = rootDir}

      val () =
          let
            val dir = mkPackagesDirectory {rootDirectory = rootDir}
          in
            createDirectory dir
          end

      val () =
          let
            val dir = mkStagingPackagesDirectory {rootDirectory = rootDir}
          in
            createDirectory dir
          end

      val () =
          let
            val dir = mkReposDirectory {rootDirectory = rootDir}
          in
            createDirectory dir
          end

      val () =
          let
            val cfg = DirectoryConfig.default

            val {filename = file} = mkConfigFilename {rootDirectory = rootDir}
          in
            DirectoryConfig.toTextFile {config = cfg, filename = file}
          end

      val () =
          let
            val file = mkInstalledFilename {rootDirectory = rootDir}
          in
            DirectoryChecksums.create file
          end
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Directories and filenames.                                                *)
(* ------------------------------------------------------------------------- *)

fun configFilename dir =
    let
      val {directory = rootDir} = root dir
    in
      mkConfigFilename {rootDirectory = rootDir}
    end;

fun installedFilename dir =
    let
      val {directory = rootDir} = root dir
    in
      mkInstalledFilename {rootDirectory = rootDir}
    end;

fun packagesDirectory dir =
    let
      val {directory = rootDir} = root dir
    in
      mkPackagesDirectory {rootDirectory = rootDir}
    end;

fun packageDirectory dir name =
    let
      val {directory = rootDir} = root dir
    in
      mkPackageDirectory {rootDirectory = rootDir} name
    end;

fun stagingPackagesDirectory dir =
    let
      val {directory = rootDir} = root dir
    in
      mkStagingPackagesDirectory {rootDirectory = rootDir}
    end;

fun stagingPackageDirectory dir name =
    let
      val {directory = rootDir} = root dir
    in
      mkStagingPackageDirectory {rootDirectory = rootDir} name
    end;

fun reposDirectory dir =
    let
      val {directory = rootDir} = root dir
    in
      mkReposDirectory {rootDirectory = rootDir}
    end;

fun repoFilename dir repo =
    let
      val {directory = rootDir} = root dir

      val name = DirectoryRepo.name repo
    in
      mkRepoFilename {rootDirectory = rootDir} name
    end;

(* ------------------------------------------------------------------------- *)
(* Package information.                                                      *)
(* ------------------------------------------------------------------------- *)

fun packageInfo dir name =
    let
      val {directory} = packageDirectory dir name
    in
      PackageInfo.mk {name = name, directory = directory}
    end;

fun stagingPackageInfo dir name =
    let
      val {directory} = stagingPackageDirectory dir name
    in
      PackageInfo.mk {name = name, directory = directory}
    end;

(* ------------------------------------------------------------------------- *)
(* Looking up packages in the package directory.                             *)
(* ------------------------------------------------------------------------- *)

fun peek dir name = DirectoryPackages.peek (packages dir) name;

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
      SOME info => PackageInfo.packages info
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
(* Updating the local package list.                                          *)
(* ------------------------------------------------------------------------- *)

fun deleteLocal dir =
    let
      val {filename} = localRepoFilename dir
    in
      OS.FileSys.remove filename
      handle OS.SysErr _ => ()
    end;

fun addLocal dir info =
    let
      val name = PackageInfo.name info
      and chk = PackageInfo.readChecksum info

      val {filename} = localRepoFilename dir

      val cmd =
          "echo \"" ^ PackageName.toString name ^ " " ^
          Checksum.toString chk ^ "\"" ^
          " >> " ^ filename

(*OpenTheoryTrace1
      val () = print (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "adding to the installed package list failed"
    in
      ()
    end;

fun updateLocal dir =
    let
      val () = deleteLocal dir

      val () =
          let
            val {directory = rootDir} = root dir
          in
            createLocal {rootDirectory = rootDir}
          end

      val () = appPackages (addLocal dir) (packages dir)
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Staging theory files for installation.                                    *)
(* ------------------------------------------------------------------------- *)

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

              val dest = Article.normalizeFilename {filename = filename}
            in
              add (src,dest) plan
            end

        fun addExtra (extra,plan) =
            let
              val name = Package.nameExtraFile extra
              and {filename} = Package.filenameExtraFile extra

              val src = {name = name, filename = SOME filename}

              val extra = Package.normalizeExtraFile extra

              val dest = Package.filenameExtraFile extra
            in
              add (src,dest) plan
            end

        val reserved =
            [("theory file", PackageInfo.theoryFile info),
             ("tarball", PackageInfo.tarball info),
             ("checksum file", PackageInfo.checksum info)]

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
                Article.normalizeFilename {filename = filename}

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
      case Package.fromTagExtraFile tag of
        NONE => tag
      | SOME extra =>
        let
          val {filename = srcFilename} = Package.filenameExtraFile extra

          val srcFilename = OS.Path.concat (srcDir,srcFilename)

          val extra = Package.normalizeExtraFile extra

          val {filename = pkgFilename} = Package.filenameExtraFile extra

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
          Package.toTagExtraFile extra
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

        val _ = not (existsFatalError errs) orelse
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

          (* Create the tarball *)

          val () = PackageInfo.createTarball stageInfo

          (* Create the checksum *)

          val () = PackageInfo.createChecksum stageInfo
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
        (* Add to the checksum list of installed packages *)

        val () = addLocal dir stageInfo

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
        val errs = checkUninstall dir name

        val _ = not (existsFatalError errs) orelse
                raise Bug "Directory.uninstall: fatal error"
*)

      val Directory {packages as ref pkgs, ...} = dir

      val info = packageInfo dir name

      (* Nuke the package directory *)

      val () = PackageInfo.nukeDirectory info

      (* Update the list of installed packages *)

      val () = packages := removePackages pkgs name

      (* Update the checksum list of installed packages *)

      val () = updateLocal dir
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Uploading packages from the package directory to a repo.                  *)
(* ------------------------------------------------------------------------- *)

fun checkUpload dir repo name =
    if not (installed dir name) then [NotInstalledError]
    else
      let
        val errs = []
      in
        errs
      end;

fun upload dir repo name =
    let
(*OpenTheoryDebug
        val errs = checkUpload dir repo name

        val _ = not (existsFatalError errs) orelse
                raise Bug "Directory.upload: fatal error"
*)

      val info = packageInfo dir name

    in
      raise Bug "Directory.upload: not implemented"
    end;

(* ------------------------------------------------------------------------- *)
(* Downloading packages from a repo to the package directory.                *)
(* ------------------------------------------------------------------------- *)

fun download dir repo pkg =
    raise Bug "Directory.download: not implemented";

(* ------------------------------------------------------------------------- *)
(* A package finder.                                                         *)
(* ------------------------------------------------------------------------- *)

fun finder dir = PackageFinder.mk (peek dir);

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap root (Print.ppBracket "<" ">" ppDirectory);

end
