(* ========================================================================= *)
(* THEORY PACKAGE DIRECTORIES                                                *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Directory :> Directory =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Directory operations.                                                     *)
(* ------------------------------------------------------------------------- *)

fun createDirectory {directory} = OS.FileSys.mkDir directory;

fun renameDirectory {src,dest} = OS.FileSys.rename {old = src, new = dest};

(* ------------------------------------------------------------------------- *)
(* File operations.                                                          *)
(* ------------------------------------------------------------------------- *)

fun ageFilename {filename} =
    let
      val mt = OS.FileSys.modTime filename
    in
      Time.- (Time.now (), mt)
    end;

(* ------------------------------------------------------------------------- *)
(* Clean up the repo package lists.                                          *)
(* ------------------------------------------------------------------------- *)

local
  fun delFile (name,{filename}) =
      let
        val () = OS.FileSys.remove filename

(*OpenTheoryTrace1
        val () = trace ("removed package list for old " ^
                        PackageName.toString name ^ " repo\n")
*)
      in
        ()
      end;
in
  fun checkReposDirectory cfgs {directory = dir} =
      let
        val dirStrm = OS.FileSys.openDir dir

        fun readAll dels utds =
            case OS.FileSys.readDir dirStrm of
              NONE => (dels,utds)
            | SOME file =>
              let
                val name =
                    case DirectoryChecksums.destFilename {filename = file} of
                      SOME n => n
                    | NONE =>
                      raise Error ("bad filename "^file^" in repos directory")

                val filename =
                    {filename = OS.Path.joinDirFile {dir = dir, file = file}}
              in
                case DirectoryConfig.findRepo cfgs name of
                  NONE => readAll ((name,filename) :: dels) utds
                | SOME cfg =>
                  let
                    val age = ageFilename filename

                    val threshold = DirectoryConfig.refreshRepo cfg

                    val utds =
                        if Time.> (age,threshold) then utds
                        else name :: utds
                  in
                    readAll dels utds
                  end
              end

        val (dels,utds) = readAll [] []

        val () = OS.FileSys.closeDir dirStrm

        val () = List.app delFile dels
      in
        utds
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Creating a new theory package directory.                                  *)
(* ------------------------------------------------------------------------- *)

local
  fun createNewDir rootDir cfg =
      let
        val () = createDirectory {directory = rootDir}

        val () =
            let
              val dir =
                  DirectoryPath.mkPackagesDirectory
                    {rootDirectory = rootDir}
            in
              createDirectory dir
            end

        val () =
            let
              val dir =
                  DirectoryPath.mkStagingPackagesDirectory
                    {rootDirectory = rootDir}
            in
              createDirectory dir
            end

        val () =
            let
              val dir =
                  DirectoryPath.mkReposDirectory
                    {rootDirectory = rootDir}
            in
              createDirectory dir
            end

        val () =
            let
              val {filename = file} =
                  DirectoryPath.mkConfigFilename
                    {rootDirectory = rootDir}
            in
              DirectoryConfig.toTextFile {config = cfg, filename = file}
            end

        val () =
            let
              val file =
                  DirectoryPath.mkInstalledFilename
                    {rootDirectory = rootDir}
            in
              DirectoryChecksums.create file
            end
      in
        ()
      end
      handle OS.SysErr (s,_) => raise Error ("system error: " ^ s);
in
  fun create {rootDirectory = rootDir, config = cfg} =
      createNewDir rootDir cfg
      handle Error err =>
        let
          val err =
              "couldn't create a new theory directory " ^
              rootDir ^ "\n" ^ err
        in
          raise Error err
        end;
end;

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
            val filename =
                DirectoryPath.mkConfigFilename {rootDirectory = rootDir}
          in
            DirectoryConfig.fromTextFile filename
          end

      val packages =
          let
            val sys = DirectoryConfig.system config
          in
            DirectoryPackages.mk
              {system = sys,
               rootDirectory = rootDir}
          end

      val repos =
          let
            val sys = DirectoryConfig.system config
            and cfgs = DirectoryConfig.repos config

            val dir =
                DirectoryPath.mkReposDirectory
                  {rootDirectory = rootDir}

            val utds = checkReposDirectory cfgs dir

            fun mkRepo cfg =
                let
                  val name = DirectoryConfig.nameRepo cfg
                  and {url} = DirectoryConfig.urlRepo cfg
                in
                  DirectoryRepo.mk
                    {system = sys,
                     name = name,
                     rootUrl = url,
                     rootDirectory = rootDir,
                     upToDate = List.exists (PackageName.equal name) utds}
                end
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

fun rootDirectory (Directory {rootDirectory = x, ...}) = {rootDirectory = x};

fun config (Directory {config = x, ...}) = x;

fun system dir = DirectoryConfig.system (config dir);

fun packages (Directory {packages = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Looking up repos in the package directory.                                *)
(* ------------------------------------------------------------------------- *)

fun repos (Directory {repos = x, ...}) = x;

fun peekRepo dir n =
    List.find (PackageName.equal n o DirectoryRepo.name) (repos dir);

fun getRepo dir n =
    case peekRepo dir n of
      SOME r => r
    | NONE =>
      let
        val err = "no repo named " ^ PackageName.toString n ^ " in config file"
      in
        raise Error err
      end;

(* ------------------------------------------------------------------------- *)
(* Looking up acceptable licenses in the package directory.                  *)
(* ------------------------------------------------------------------------- *)

fun licenses dir = DirectoryConfig.licenses (config dir);

fun peekLicense dir n = DirectoryConfig.findLicense (licenses dir) n;

fun knownLicense dir n = Option.isSome (peekLicense dir n);

fun getLicense dir n =
    case peekLicense dir n of
      SOME l => l
    | NONE =>
      let
        val {name} = n

        val err = "no license named " ^ name ^ " in config file"
      in
        raise Error err
      end;

(* ------------------------------------------------------------------------- *)
(* Paths.                                                                    *)
(* ------------------------------------------------------------------------- *)

fun configFilename dir =
    DirectoryPath.mkConfigFilename (rootDirectory dir);

fun installedFilename dir =
    DirectoryPath.mkInstalledFilename (rootDirectory dir);

fun packagesDirectory dir =
    DirectoryPath.mkPackagesDirectory (rootDirectory dir);

fun packageDirectory dir name =
    DirectoryPath.mkPackageDirectory (rootDirectory dir) name;

fun stagingPackagesDirectory dir =
    DirectoryPath.mkStagingPackagesDirectory (rootDirectory dir);

fun stagingPackageDirectory dir name =
    DirectoryPath.mkStagingPackageDirectory (rootDirectory dir) name;

fun reposDirectory dir =
    DirectoryPath.mkReposDirectory (rootDirectory dir);

fun repoFilename dir repo =
    DirectoryPath.mkRepoFilename (rootDirectory dir) repo;

(* ------------------------------------------------------------------------- *)
(* Package information.                                                      *)
(* ------------------------------------------------------------------------- *)

fun packageInfo dir namever =
    let
      val sys = system dir
      and {directory} = packageDirectory dir namever
    in
      PackageInfo.mk
        {system = sys,
         nameVersion = namever,
         directory = directory}
    end;

fun stagingPackageInfo dir namever =
    let
      val sys = system dir
      and {directory} = stagingPackageDirectory dir namever
    in
      PackageInfo.mk
        {system = sys,
         nameVersion = namever,
         directory = directory}
    end;

(* ------------------------------------------------------------------------- *)
(* Looking up packages in the package directory.                             *)
(* ------------------------------------------------------------------------- *)

fun peek dir namever = DirectoryPackages.peek (packages dir) namever;

fun get dir namever =
    case peek dir namever of
      SOME info => info
    | NONE => raise Error "Directory.get";

fun member namever dir = Option.isSome (peek dir namever);

fun checksum dir namever = DirectoryPackages.checksum (packages dir) namever;

(* ------------------------------------------------------------------------- *)
(* Looking up the latest version of packages.                                *)
(* ------------------------------------------------------------------------- *)

fun latestVersion dir namever =
    DirectoryPackages.latestVersion (packages dir) namever;

fun isLatestVersion dir namever =
    DirectoryPackages.isLatestVersion (packages dir) namever;

(* ------------------------------------------------------------------------- *)
(* Dependencies in the package directory.                                    *)
(* ------------------------------------------------------------------------- *)

fun parents dir namever =
    DirectoryPackages.parents (packages dir) namever;

fun children dir namever =
    DirectoryPackages.children (packages dir) namever;

fun ancestors dir namever =
    DirectoryPackages.ancestors (packages dir) namever;

fun descendents dir namever =
    DirectoryPackages.descendents (packages dir) namever;

(* Set versions *)

fun ancestorsSet dir namevers =
    DirectoryPackages.ancestorsSet (packages dir) namevers;

fun descendentsSet dir namevers =
    DirectoryPackages.descendentsSet (packages dir) namevers;

(* Auxiliary packages *)

fun auxiliaryParents dir namever =
    DirectoryPackages.auxiliaryParents (packages dir) namever;

fun auxiliaryChildren dir namever =
    DirectoryPackages.auxiliaryChildren (packages dir) namever;

fun auxiliaryAncestors dir namever =
    DirectoryPackages.auxiliaryAncestors (packages dir) namever;

fun auxiliaryDescendents dir namever =
    DirectoryPackages.auxiliaryDescendents (packages dir) namever;

fun auxiliaryAncestorsSet dir namevers =
    DirectoryPackages.auxiliaryAncestorsSet (packages dir) namevers;

fun auxiliaryDescendentsSet dir namevers =
    DirectoryPackages.auxiliaryDescendentsSet (packages dir) namevers;

fun isAuxiliary dir namever =
    DirectoryPackages.isAuxiliary (packages dir) namever;

(* ------------------------------------------------------------------------- *)
(* Arranging packages in installation order.                                 *)
(* ------------------------------------------------------------------------- *)

fun installOrder dir namevers =
    DirectoryPackages.installOrder (packages dir) namevers;

fun installOrdered dir namevers =
    DirectoryPackages.installOrdered (packages dir) namevers;

(* ------------------------------------------------------------------------- *)
(* Listing packages in the package directory.                                *)
(* ------------------------------------------------------------------------- *)

fun list dir = DirectoryPackages.list (packages dir);

(* ------------------------------------------------------------------------- *)
(* Upgrading theory packages.                                                *)
(* ------------------------------------------------------------------------- *)

fun upgrade dir pkg =
    let
      val pkgs = packages dir

      fun latest nv =
          let
            val () =
                if DirectoryPackages.member nv pkgs then ()
                else
                  let
                    val mesg =
                        "package " ^ PackageNameVersion.toString nv ^
                        " is not installed"
                  in
                    warn mesg
                  end

            val nvs = DirectoryPackages.latestVersion pkgs nv

            val () =
                if PackageNameVersionSet.size nvs <= 1 then ()
                else
                  let
                    val err =
                        "multiple upgrade paths for package " ^
                        PackageNameVersion.toString nv
                  in
                    raise Error err
                  end

            val nv' =
                case PackageNameVersionSet.findl (K true) nvs of
                  SOME nv' => nv'
                | NONE =>
                  let
                    val err =
                        "no versions of package " ^
                        PackageName.toString (PackageNameVersion.name nv) ^
                        " are installed"
                  in
                    raise Error err
                  end
          in
            if PackageNameVersion.equal nv' nv then NONE else SOME nv'
          end
    in
      Package.updatePackages latest pkg
    end;

(* ------------------------------------------------------------------------- *)
(* A package finder and importer.                                            *)
(* ------------------------------------------------------------------------- *)

fun finder dir = PackageFinder.mk (peek dir);

fun importer dir = TheoryGraph.fromFinderImporter (finder dir);

(* ------------------------------------------------------------------------- *)
(* A package finder for *staged* packages.                                   *)
(* ------------------------------------------------------------------------- *)

fun stagedFinder dir =
    let
      fun stagedPeek namever =
          let
(*OpenTheoryTrace3
            val () = Print.trace PackageNameVersion.pp
                       "Directory.stagedFinder: namever" namever
*)
            val info = stagingPackageInfo dir namever

            val result =
                if PackageInfo.existsDirectory info then SOME info else NONE

(*OpenTheoryTrace3
            val () = Print.trace Print.ppBool
                       "Directory.stagedFinder: found" (Option.isSome result)
*)
          in
            result
          end
    in
      PackageFinder.mk stagedPeek
    end;

(* ------------------------------------------------------------------------- *)
(* Checking package tags.                                                    *)
(* ------------------------------------------------------------------------- *)

local
  fun getTag name tags errs =
      case PackageTag.filterName name tags of
        [] =>
        let
          val err = DirectoryError.TagError (name,"is missing")
        in
          (err :: errs, NONE)
        end
      | [v] => (errs, SOME v)
      | _ :: _ :: _ =>
        let
          val err = DirectoryError.TagError (name,"is declared multiple times")
        in
          (err :: errs, NONE)
        end;

  fun checkNameTag namever tags errs =
      let
        val name = PackageName.nameTag

        val (errs,so) = getTag name tags errs
      in
        case so of
          NONE => errs
        | SOME s =>
          case total PackageName.fromString s of
            NONE =>
            let
              val msg = "is badly formatted: " ^ s

              val err = DirectoryError.TagError (name,msg)
            in
              err :: errs
            end
          | SOME n =>
            let
              val n' = PackageNameVersion.name namever
            in
              if PackageName.equal n n' then errs
              else
                let
                  val msg = "does not match"

                  val err = DirectoryError.TagError (name,msg)
                in
                  err :: errs
                end
            end
      end;

  fun checkVersionTag namever tags errs =
      let
        val name = PackageName.versionTag

        val (errs,so) = getTag name tags errs
      in
        case so of
          NONE => errs
        | SOME s =>
          case total PackageVersion.fromString s of
            NONE =>
            let
              val msg = "is badly formatted: " ^ s

              val err = DirectoryError.TagError (name,msg)
            in
              err :: errs
            end
          | SOME v =>
            let
              val v' = PackageNameVersion.version namever
            in
              if PackageVersion.equal v v' then errs
              else
                let
                  val msg = "does not match"

                  val err = DirectoryError.TagError (name,msg)
                in
                  err :: errs
                end
            end
      end;

  fun checkDescriptionTag tags errs =
      let
        val name = PackageName.descriptionTag

        val (errs,so) = getTag name tags errs
      in
        case so of
          NONE => errs
        | SOME s =>
          if s <> "" then errs
          else
            let
              val msg = "is blank"

              val err = DirectoryError.TagError (name,msg)
            in
              err :: errs
            end
      end;

  fun checkAuthorTag tags errs =
      let
        val name = PackageName.authorTag

        val (errs,so) = getTag name tags errs
      in
        case so of
          NONE => errs
        | SOME s =>
          if s <> "" then errs
          else
            let
              val msg = "is blank"

              val err = DirectoryError.TagError (name,msg)
            in
              err :: errs
            end
      end;

  fun checkLicenseTag dir tags errs =
      let
        val name = PackageName.licenseTag

        val (errs,so) = getTag name tags errs
      in
        case so of
          NONE => errs
        | SOME s =>
          if knownLicense dir {name = s} then errs
          else
            let
              val msg = s ^ " is not acceptable"

              val err = DirectoryError.TagError (name,msg)
            in
              err :: errs
            end
      end;
in
  fun checkTags dir namever tags =
      let
        val errs = []

        val errs = checkNameTag namever tags errs

        val errs = checkVersionTag namever tags errs

        val errs = checkDescriptionTag tags errs

        val errs = checkAuthorTag tags errs

        val errs = checkLicenseTag dir tags errs
      in
        errs
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Summarizing packages.                                                     *)
(* ------------------------------------------------------------------------- *)

fun summary impt info =
    let
      val graph = TheoryGraph.empty {savable = false}

      val imps = TheorySet.empty

      val int = Interpretation.natural

      val (_,thy) =
          TheoryGraph.importPackageInfo impt graph
            {imports = imps,
             interpretation = int,
             info = info}
    in
      TheoryGraph.summary thy
    end;

(* ------------------------------------------------------------------------- *)
(* Post-stage functions.                                                     *)
(* ------------------------------------------------------------------------- *)

fun postStagePackage dir fndr stageInfo warnSummary {tool} =
    let
      (* Check the package tags *)

      val pkg = PackageInfo.package stageInfo

      val () =
          let
            val namever = PackageInfo.nameVersion stageInfo

            val tags = Package.tags pkg

            val errs = checkTags dir namever tags
          in
            if not (DirectoryError.existsFatal errs) then ()
            else raise Error (DirectoryError.toStringList errs)
          end

      (* Check the package summary *)

      val sum =
          let
            val impt = TheoryGraph.fromFinderImporter fndr
          in
            summary impt stageInfo
          end;

      val () =
          if not warnSummary then ()
          else PackageSummary.check (Package.show pkg) sum

      (* Create the package document *)

      val () =
          let
            val files =
                let
                  val {filename = theory} = PackageInfo.theoryFile stageInfo
                  and {filename = tarball} = PackageInfo.tarball stageInfo
                in
                  {theory = theory, tarball = tarball}
                end

            val doc =
                PackageDocument.mk
                  (PackageDocument.Document'
                     {package = pkg,
                      summary = sum,
                      files = files,
                      tool = tool})
          in
            PackageInfo.writeDocument stageInfo doc
          end
    in
      ()
    end;

fun postStageTarball dir fndr stageInfo contents tool =
    let
      val minimal =
          let
            val cfg = DirectoryConfig.install (config dir)
          in
            {minimal = DirectoryConfig.minimalInstall cfg}
          end

      (* Unpack the tarball *)

      val () = PackageInfo.unpackTarball stageInfo contents minimal

      (* Check the required packages are installed *)

      val pars = PackageInfo.packages stageInfo

      val () = PackageNameVersionSet.app (PackageFinder.check fndr) pars

      (* Common post-stage operations *)

      val () = postStagePackage dir fndr stageInfo false tool
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Staging packages for installation.                                        *)
(* ------------------------------------------------------------------------- *)

fun checkStagePackage dir repo namever chk =
    if member namever dir then [DirectoryError.AlreadyInstalled namever]
    else
      let
        val errs = []

        val errs =
            let
              val stageInfo = stagingPackageInfo dir namever
            in
              if not (PackageInfo.existsDirectory stageInfo) then errs
              else DirectoryError.AlreadyStaged namever :: errs
            end

        val errs =
            case DirectoryRepo.peek repo namever of
              NONE =>
              DirectoryError.NotOnRepo (namever,repo) :: errs
            | SOME chk' =>
              if Checksum.equal chk' chk then errs
              else DirectoryError.WrongChecksumOnRepo (namever,repo) :: errs
      in
        List.rev errs
      end;

fun stagePackage dir fndr repo namever chk tool =
    let
(*OpenTheoryDebug
      val errs = checkStagePackage dir repo namever chk

      val _ = not (DirectoryError.existsFatal errs) orelse
              raise Bug "Directory.stagePackage: fatal error"
*)
      (* Make a package info for the stage directory *)

      val stageInfo = stagingPackageInfo dir namever

      (* Create the stage directory *)

      val () = PackageInfo.createDirectory stageInfo
    in
      let
        (* Download the package tarball *)

        val () = DirectoryRepo.download repo stageInfo

        (* List the contents of the tarball *)

        val contents = PackageInfo.contentsTarball stageInfo

        (* Common post-stage operations *)

        val () = postStageTarball dir fndr stageInfo contents tool
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
(* Staging tarballs for installation.                                        *)
(* ------------------------------------------------------------------------- *)

fun checkStageTarball dir contents =
    let
      val PackageTarball.Contents {nameVersion = namever, ...} = contents
    in
      if member namever dir then [DirectoryError.AlreadyInstalled namever]
      else
        let
          val errs = []

          val errs =
              let
                val stageInfo = stagingPackageInfo dir namever
              in
                if not (PackageInfo.existsDirectory stageInfo) then errs
                else DirectoryError.AlreadyStaged namever :: errs
              end
        in
          errs
        end
    end;

fun stageTarball dir fndr tarFile contents tool =
    let
(*OpenTheoryDebug
      val errs = checkStageTarball dir contents

      val _ = not (DirectoryError.existsFatal errs) orelse
              raise Bug "Directory.stageTarball: fatal error"
*)
      val PackageTarball.Contents {nameVersion = namever, ...} = contents

      (* Make a package info for the stage directory *)

      val stageInfo = stagingPackageInfo dir namever

      (* Create the stage directory *)

      val () = PackageInfo.createDirectory stageInfo
    in
      let
        (* Copy the package tarball *)

        val () = PackageInfo.copyTarball stageInfo tarFile

        (* Common post-stage operations *)

        val () = postStageTarball dir fndr stageInfo contents tool
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
(* Staging theory files for installation.                                    *)
(* ------------------------------------------------------------------------- *)

local
  fun checkDep dir (namever,errs) =
      if member namever dir then errs
      else DirectoryError.UninstalledParent namever :: errs;

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
              val PackageExtra.Extra {name,filename} = PackageExtra.dest extra

              val src =
                  {name = "extra " ^ PackageName.toString name ^ " file",
                   filename = SOME filename}

              val extra = PackageExtra.normalize extra

              val dest = PackageExtra.filename extra
            in
              add (src,dest) plan
            end

        val reserved =
            [("theory file", PackageInfo.theoryFile info),
             ("tarball", PackageInfo.tarball info),
             ("document", PackageInfo.document info)]

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
                | SOME sf => [Print.ppString " ", Print.ppString sf]))

        fun ppCopy (dest,srcs) =
            Print.consistentBlock 2
              (Print.ppString dest ::
               Print.ppString ":" ::
               List.map (Print.sequence Print.newline o ppSrc) srcs)
      in
        fn plan =>
           case StringMap.toList plan of
             [] => Print.skip
           | cp :: cps =>
             Print.consistentBlock 0
               (ppCopy cp ::
                List.map (Print.sequence Print.newline o ppCopy) cps)
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
                DirectoryError.FilenameClash {srcs = srcs, dest = dest} :: errs
              end
      in
        StringMap.foldl check
      end;
in
  fun checkStageTheory dir namever pkg =
      let
        val errs = []

        val errs =
            if not (member namever dir) then errs
            else DirectoryError.AlreadyInstalled namever :: errs

        val errs =
            let
              val stageInfo = stagingPackageInfo dir namever
            in
              if not (PackageInfo.existsDirectory stageInfo) then errs
              else DirectoryError.AlreadyStaged namever :: errs
            end

        val errs = checkTags dir namever (Package.tags pkg) @ errs

        val errs = List.foldl (checkDep dir) errs (Package.packages pkg)

        val info = packageInfo dir namever

        val plan = mkFileCopyPlan info pkg

(*OpenTheoryTrace1
        val () =
            Print.trace ppFileCopyPlan "Directory.checkStageTheory: plan" plan
*)

        val errs = checkFileCopyPlan errs plan
      in
        List.rev errs
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

(*OpenTheoryTrace1
            val () =
                Print.trace Print.ppString
                  "Directory.stageTheory.copyArticle: srcFilename" srcFilename
*)

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

(*OpenTheoryTrace1
            val () =
                Print.trace Print.ppString
                  "Directory.stageTheory.copyArticle: destFilename" destFilename
*)

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

  fun copyExtraFile sys srcDir info tag =
      case PackageTag.toExtra tag of
        NONE => tag
      | SOME extra =>
        let
          val {filename = srcFilename} = PackageExtra.filename extra

(*OpenTheoryTrace1
          val () =
              Print.trace Print.ppString
                "Directory.stageTheory.copyExtraFile: srcFilename" srcFilename
*)

          val srcFilename = OS.Path.concat (srcDir,srcFilename)

          val extra = PackageExtra.normalize extra

          val {filename = pkgFilename} = PackageExtra.filename extra

          val {filename = destFilename} =
              PackageInfo.joinDirectory info {filename = pkgFilename}

          val {cp = cmd} = DirectorySystem.cp sys

          val cmd = cmd ^ " " ^ srcFilename ^ " " ^ destFilename

(*OpenTheoryTrace1
          val () = trace (cmd ^ "\n")
*)

          val () =
              if OS.Process.isSuccess (OS.Process.system cmd) then ()
              else raise Error "copying extra file failed"
        in
          PackageTag.fromExtra extra
        end;

  fun copyArticles srcDir info pkg =
      let
        val Package.Package' {tags,theories} = Package.dest pkg

        val theories = List.map (copyArticle srcDir info) theories
      in
        Package.mk (Package.Package' {tags = tags, theories = theories})
      end;

  fun copyExtraFiles sys srcDir info pkg =
      let
        val Package.Package' {tags,theories} = Package.dest pkg

        val tags = List.map (copyExtraFile sys srcDir info) tags
      in
        Package.mk (Package.Package' {tags = tags, theories = theories})
      end;

  fun checkTheory dir info pkg =
      let
(*OpenTheoryTrace1
        val () = trace "Directory.stageTheory.checkTheory\n"
*)

        val Package.Package' {tags,theories} = Package.dest pkg

        val impt = importer dir
        and {directory = pdir} = PackageInfo.directory info

        val thys =
            PackageDag.mk
              {importer = impt,
               directory = pdir,
               theories = theories}

        val thys = PackageDag.unwind thys

        val theories = PackageDag.theories thys
      in
        Package.mk (Package.Package' {tags = tags, theories = theories})
      end;

  fun writeTheoryFile stageInfo pkg =
      let
(*OpenTheoryTrace1
        val () = trace "Directory.stageTheory.writeTheoryFile\n"
*)

        val file = PackageInfo.theoryFile stageInfo

        val {filename} = PackageInfo.joinDirectory stageInfo file
      in
        Package.toTextFile {package = pkg, filename = filename}
      end;
in
  fun stageTheory dir namever pkg {directory = srcDir} tool =
      let
(*OpenTheoryDebug
        val errs = checkStageTheory dir namever pkg

        val _ = not (DirectoryError.existsFatal errs) orelse
                raise Bug "Directory.stageTheory: fatal error"
*)
        val sys = system dir

        (* Make a package info for the stage directory *)

        val stageInfo = stagingPackageInfo dir namever

        (* Create the stage directory *)

        val () = PackageInfo.createDirectory stageInfo
      in
        let
          (* Copy the articles over *)

          val pkg = copyArticles srcDir stageInfo pkg

          (* Copy the extra files over *)

          val pkg = copyExtraFiles sys srcDir stageInfo pkg

          (* Check the package theory *)

          val pkg = checkTheory dir stageInfo pkg

          (* Write the new theory file *)

          val () = writeTheoryFile stageInfo pkg

          (* Create the tarball *)

          val () = PackageInfo.createTarball stageInfo

          (* Common post-stage operations *)

          val fndr = finder dir

          val () = postStagePackage dir fndr stageInfo true tool
        in
          PackageInfo.checksumTarball stageInfo
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

fun checkInstallStaged dir namever chk =
    let
      val stageInfo = stagingPackageInfo dir namever

      val pkgInfo = packageInfo dir namever

      val errs = []

      val errs =
          if PackageInfo.existsDirectory stageInfo then errs
          else DirectoryError.NotStaged namever :: errs

      val errs =
          if not (PackageInfo.existsDirectory pkgInfo) then errs
          else DirectoryError.AlreadyInstalled namever :: errs

      val errs =
          if not (List.null errs) then errs
          else
            let
              fun add (dep,acc) =
                  if member dep dir then acc
                  else DirectoryError.UninstalledParent dep :: acc

              val pkg = PackageInfo.package stageInfo
            in
              List.foldl add errs (Package.packages pkg)
            end
    in
      List.rev errs
    end;

fun installStaged dir namever chk =
    let
      val stageInfo = stagingPackageInfo dir namever

      val pkgInfo = packageInfo dir namever

      val () =
          if PackageInfo.existsDirectory stageInfo then ()
          else raise Error "staged package directory does not exist"

      val () =
          if not (PackageInfo.existsDirectory pkgInfo) then ()
          else raise Error "package directory already exists"
    in
      let
        (* Rename staged package directory to package directory *)

        val {directory = stageDir} = PackageInfo.directory stageInfo

        val {directory = pkgDir} = PackageInfo.directory pkgInfo

        val () = renameDirectory {src = stageDir, dest = pkgDir}

        (* Update the list of installed packages *)

        val Directory {packages = pkgs, ...} = dir

        val () = DirectoryPackages.add pkgs pkgInfo chk
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
(* Cleaning up staged packages.                                              *)
(* ------------------------------------------------------------------------- *)

fun listStaged dir {maxAge} =
    let
      val {directory = stagingDir} = stagingPackagesDirectory dir

      val dirStrm = OS.FileSys.openDir stagingDir

      fun readAll dels =
          case OS.FileSys.readDir dirStrm of
            NONE => dels
          | SOME file =>
            let
              val namever = PackageNameVersion.fromString file

              val young =
                  case maxAge of
                    NONE => false
                  | SOME threshold =>
                    let
                      val directory =
                          OS.Path.joinDirFile {dir = stagingDir, file = file}

                      val age = ageFilename {filename = directory}
                    in
                      Time.<= (age,threshold)
                    end

              val dels =
                  if young then dels
                  else PackageNameVersionSet.add dels namever
            in
              readAll dels
            end

      val dels = readAll PackageNameVersionSet.empty

      val () = OS.FileSys.closeDir dirStrm
    in
      dels
    end;

fun cleanupStaged dir namever =
    let
      val stageInfo = stagingPackageInfo dir namever

      val () = PackageInfo.nukeDirectory stageInfo
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Uninstalling packages from the package directory.                         *)
(* ------------------------------------------------------------------------- *)

fun checkUninstall dir namever =
    if not (member namever dir) then [DirectoryError.NotInstalled namever]
    else
      let
        val errs = []

        val desc = descendents dir namever

        val errs =
            if PackageNameVersionSet.null desc then errs
            else
              let
                fun add (n,acc) = DirectoryError.InstalledDescendent n :: acc
              in
                PackageNameVersionSet.foldl add errs desc
              end
      in
        List.rev errs
      end;

fun uninstall dir namever =
    let
(*OpenTheoryDebug
      val errs = checkUninstall dir namever

      val _ = not (DirectoryError.existsFatal errs) orelse
              raise Bug "Directory.uninstall: fatal error"
*)

      val Directory {packages = pkgs, ...} = dir

      val info = packageInfo dir namever

      (* Nuke the package directory *)

      val () = PackageInfo.nukeDirectory info

      (* Delete from the list of installed packages *)

      val () = DirectoryPackages.delete pkgs namever
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Uploading packages from the package directory to a repo.                  *)
(* ------------------------------------------------------------------------- *)

local
  fun collectAuthors dir =
      let
        fun add (nv,acc) =
            let
              val info = get dir nv

              val pkg = PackageInfo.package info

              val {author} = Package.author pkg
            in
              StringMap.insert acc (author,nv)
            end

        fun mk (auth,nv) = (nv, {author = auth})
      in
        fn nvs =>
           let
             val auths = List.foldl add (StringMap.new ()) nvs
           in
             List.map mk (StringMap.toList auths)
           end
      end;

  fun allInstalled dir nvs = List.all (fn nv => member nv dir) nvs;

  fun checkInstalled dir =
      let
        fun isKnown nv = member nv dir

        fun add (nv,acc) = DirectoryError.NotInstalled nv :: acc
      in
        fn namevers => fn errs =>
           let
             val (namevers,unknown) = List.partition isKnown namevers

             val errs = List.foldl add errs unknown
           in
             (namevers,errs)
           end
      end;

  fun checkNotOnRepo repo =
      let
        fun check (nv,errs) =
            if not (DirectoryRepo.member nv repo) then errs
            else DirectoryError.AlreadyOnRepo (nv,repo) :: errs
      in
        fn namevers => fn errs => List.foldl check errs namevers
      end;

  fun checkAncestorsOnRepo dir repo =
      let
        fun addMissing anc errs =
            DirectoryError.AncestorNotOnRepo (anc,repo) :: errs

        fun addDifferent anc errs =
            DirectoryError.AncestorWrongChecksumOnRepo (anc,repo) :: errs

        fun checkAnc (anc,errs) =
            let
              val chk =
                  case checksum dir anc of
                    SOME c => c
                  | NONE =>
                    let
                      val err =
                          "depends on package " ^
                          PackageNameVersion.toString anc ^
                          " which seems to be badly installed"
                    in
                      raise Error err
                    end
            in
              case DirectoryRepo.peek repo anc of
                NONE => addMissing anc errs
              | SOME chk' =>
                if Checksum.equal chk chk' then errs
                else addDifferent anc errs
            end
      in
        fn namevers => fn errs =>
           let
             val nvs = PackageNameVersionSet.fromList namevers

             val ancs = ancestorsSet dir nvs

             val ancs = PackageNameVersionSet.difference ancs nvs
           in
             PackageNameVersionSet.foldl checkAnc errs ancs
           end
      end;

  fun checkSameAuthor dir namevers errs =
      let
        val auths = collectAuthors dir (List.rev namevers)
      in
        case auths of
          [] => raise Bug "Directory.checkUpload.checkSameAuthor"
        | [(_,auth)] => (auth,errs)
        | (_,auth) :: _ :: _ =>
          (auth, DirectoryError.MultipleAuthors auths :: errs)
      end;

  fun checkObsoleteInstalled dir repo =
      let
        fun check (nv,(obs,errs)) =
            case DirectoryRepo.previousVersion repo nv of
              NONE => (obs,errs)
            | SOME (nv',chk') =>
              case checksum dir nv' of
                NONE =>
                let
                  val err =
                      DirectoryError.UninstalledObsolete
                        {upload = nv,
                         obsolete = nv'}
                in
                  (obs, err :: errs)
                end
              | SOME chk =>
                if Checksum.equal chk chk' then (nv' :: obs, errs)
                else
                  let
                    val err =
                        DirectoryError.WrongChecksumObsolete
                          {upload = nv,
                           obsolete = nv'}
                  in
                    (obs, err :: errs)
                  end
      in
        fn namevers => fn errs => List.foldl check ([],errs) namevers
      end;

  fun checkObsoleteAuthors dir author obsolete errs =
      let
        fun notAuthor (_,auth) = auth <> author

        val auths = collectAuthors dir obsolete

        val auths = List.filter notAuthor auths
      in
        if List.null auths then errs
        else DirectoryError.ObsoleteAuthors auths :: errs
      end;
in
  fun checkUpload dir {repo, support, packages = namevers} =
      let
        val errs = []

        (* Check there exist upload packages *)

        val () =
            if not (List.null namevers) then ()
            else raise Bug "Directory.checkUpload: no upload packages"

        (* Check upload packages are installed *)

        val () =
            if allInstalled dir support then ()
            else raise Bug "Directory.checkUpload: support not installed"

        val (namevers,errs) = checkInstalled dir namevers errs
      in
        if List.null namevers then List.rev errs
        else
          let
            (* Check upload packages are in install order *)

            val () =
                if installOrdered dir (support @ namevers) then ()
                else raise Bug "Directory.checkUpload: not in install order"

            (* Check upload packages are not installed on the repo *)

            val errs = checkNotOnRepo repo (support @ namevers) errs

            (* Check upload ancestor packages are installed on the repo *)

            val errs = checkAncestorsOnRepo dir repo (support @ namevers) errs

            (* Check upload packages have the same author *)

            val (author,errs) = checkSameAuthor dir namevers errs

            (* Warn if obsolete packages are not installed *)

            val (obsolete,errs) = checkObsoleteInstalled dir repo namevers errs

            (* Warn about obsoleting packages by other authors *)

            val errs = checkObsoleteAuthors dir author obsolete errs
          in
            List.rev errs
          end
      end;
end;

fun supportUpload dir upl namever =
    let
      val chk =
          case checksum dir namever of
            SOME c => c
          | NONE =>
            let
              val err =
                  "package " ^ PackageNameVersion.toString namever ^
                  " seems to be badly installed"
            in
              raise Error err
            end

      val () = DirectoryRepo.supportUpload upl namever chk
    in
      ()
    end;

fun packageUpload dir upl namever =
    let
      val info = get dir namever

      val chk =
          case checksum dir namever of
            SOME c => c
          | NONE =>
            let
              val err =
                  "package " ^ PackageNameVersion.toString namever ^
                  " seems to be badly installed"
            in
              raise Error err
            end

      val () = DirectoryRepo.packageUpload upl info chk
    in
      ()
    end;

fun ppUpload dir {repo,support,packages} =
    let
      fun ppStep step pps =
          Print.inconsistentBlock 3
            (Print.ppInt step ::
             Print.ppString ". " ::
             pps)

      val ppNameVer = PackageNameVersion.pp

      fun ppNameVers nv nvs =
          ppNameVer nv ::
          List.map (Print.sequence Print.break o ppNameVer) nvs

      fun ppAuthor {author} = Print.ppString author

      val step = 0

      val (step,ppSupport) =
          case support of
            [] => (step,Print.skip)
          | nv :: nvs =>
            let
              val step = step + 1

              val num = length nvs + 1

              val mesg = "Request installation of "

              val ppNum =
                  if num = 1 then Print.ppString "a support package:"
                  else
                    Print.sequence (Print.ppInt num)
                      (Print.ppString " support packages:")

              val pp =
                  ppStep step
                    (Print.ppString mesg ::
                     ppNum ::
                     Print.newline ::
                     ppNameVers nv nvs)
            in
              (step, Print.sequence Print.newline pp)
            end

      val (author,step,ppPackages) =
          case packages of
            [] => raise Bug "Directory.ppUpload: no packages"
          | nv :: nvs =>
            let
              val author =
                  let
                    val info = get dir nv

                    val pkg = PackageInfo.package info
                  in
                    Package.author pkg
                  end

              val step = step + 1

              val num = length nvs + 1

              val mesg = "Upload "

              val ppNum =
                  if num = 1 then Print.ppString "the package:"
                  else
                    Print.sequence (Print.ppInt num)
                      (Print.ppString " packages:")

              val pp =
                  ppStep step
                    (Print.ppString mesg ::
                     ppNum ::
                     Print.newline ::
                     ppNameVers nv nvs)
            in
              (author,step,pp)
            end

      val (step,ppAuthorConfirm) =
          let
            val step = step + 1

            val mesg = "Send a confirmation email to the package author:"

            val pp =
                ppStep step
                  [Print.ppString mesg,
                   Print.newline,
                   ppAuthor author]
          in
            (step,pp)
          end

      val ppRepo =
          Print.inconsistentBlock 2
            [Print.ppString "About to upload to ",
             DirectoryRepo.pp repo,
             Print.ppString " in ",
             Print.ppInt step,
             Print.ppString " steps"]
    in
      Print.inconsistentBlock 0
        [ppRepo,
         ppSupport,
         Print.newline,
         ppPackages,
         Print.newline,
         ppAuthorConfirm]
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun pp dir =
    let
      val {rootDirectory = rootDir} = rootDirectory dir
    in
      Print.ppBracket "<" ">" Print.ppString rootDir
    end;

end
