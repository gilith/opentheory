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
(* Clean up the package staging area.                                        *)
(* ------------------------------------------------------------------------- *)

fun nukeStaged info =
    let
      val () = PackageInfo.nukeDirectory info

(*OpenTheoryTrace1
      val () =
          let
            val mesg =
                "nuked old package " ^
                PackageNameVersion.toString (PackageInfo.nameVersion info) ^
                " in staging area\n"
          in
            trace mesg
          end
*)
    in
      ()
    end;

fun checkStagingPackagesDirectory cfg {directory = dir} =
    let
      val dirStrm = OS.FileSys.openDir dir

      fun readAll dels =
          case OS.FileSys.readDir dirStrm of
            NONE => dels
          | SOME file =>
            let
              val namever = PackageNameVersion.fromString file

              val directory = OS.Path.joinDirFile {dir = dir, file = file}

              val age = ageFilename {filename = directory}

              val threshold =
                  DirectoryConfig.cleanupInstall
                    (DirectoryConfig.install cfg)

              val dels =
                  if Time.<= (age,threshold) then dels
                  else
                    let
                      val sys = DirectoryConfig.system cfg

                      val info =
                          PackageInfo.mk
                            {system = sys,
                             nameVersion = namever,
                             directory = directory}
                    in
                      info :: dels
                    end
            in
              readAll dels
            end

      val dels = readAll []

      val () = OS.FileSys.closeDir dirStrm

      val () = List.app nukeStaged dels
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Clean up the repo package lists.                                          *)
(* ------------------------------------------------------------------------- *)

local
  fun delFile (name,{filename}) =
      let
        val () = OS.FileSys.remove filename

(*OpenTheoryTrace1
        val () = trace ("removed package list for old " ^ name ^ " repo\n")
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

fun create {rootDirectory = rootDir} =
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
            val cfg = DirectoryConfig.default

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

      val () =
          let
            val dir =
                DirectoryPath.mkStagingPackagesDirectory
                  {rootDirectory = rootDir}

            val () = checkStagingPackagesDirectory config dir
          in
            ()
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

(* ------------------------------------------------------------------------- *)
(* Generate a valid installation order.                                      *)
(* ------------------------------------------------------------------------- *)

fun installOrder dir namevers =
    DirectoryPackages.installOrder (packages dir) namevers;

(* ------------------------------------------------------------------------- *)
(* Listing packages in the package directory.                                *)
(* ------------------------------------------------------------------------- *)

fun list dir = DirectoryPackages.list (packages dir);

(* ------------------------------------------------------------------------- *)
(* Upgrading theory packages.                                                *)
(* ------------------------------------------------------------------------- *)

fun checkUpgradeTheory dir pkg =
    let
      fun notInstalled p = not (member p dir)

      val missing = List.filter notInstalled (Package.packages pkg)
    in
      List.map DirectoryError.NotInstalled missing
    end;

fun upgradeTheory dir pkg =
    let
(*OpenTheoryDebug
      val errs = checkUpgradeTheory dir pkg

      val _ = not (DirectoryError.existsFatal errs) orelse
              raise Bug "Directory.upgradeTheory: fatal error"
*)
      val pkgs = packages dir

      fun latest nv =
          let
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
          in
            case PackageNameVersionSet.findl (K true) nvs of
              SOME nv' =>
              if PackageNameVersion.equal nv' nv then NONE else SOME nv'
            | NONE =>
              raise Bug "Directory.upgradeTheory.latest"
          end
    in
      Package.updatePackages latest pkg
    end;

(* ------------------------------------------------------------------------- *)
(* A package finder.                                                         *)
(* ------------------------------------------------------------------------- *)

fun finder dir = PackageFinder.mk (peek dir);

(* ------------------------------------------------------------------------- *)
(* A package importer.                                                       *)
(* ------------------------------------------------------------------------- *)

fun importer dir = Graph.fromFinderImporter (finder dir);

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

fun summary dir info =
    let
      val graph = Graph.empty {savable = false}

      val impt = importer dir

      val imps = TheorySet.empty

      val int = Interpretation.natural

      val (_,thy) =
          Graph.importPackageInfo impt graph
            {imports = imps,
             interpretation = int,
             info = info}
    in
      Graph.summary thy
    end;

(* ------------------------------------------------------------------------- *)
(* Post-stage functions.                                                     *)
(* ------------------------------------------------------------------------- *)

fun postStagePackage dir stageInfo warnSummary =
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

      val sum = summary dir stageInfo

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
                      files = files})
          in
            PackageInfo.writeDocument stageInfo doc
          end
    in
      ()
    end;

fun postStageTarball dir fndr stageInfo contents minimal =
    let
      (* Unpack the tarball *)

      val () = PackageInfo.unpackTarball stageInfo contents minimal

      (* Check the required packages are installed *)

      val pars = PackageInfo.packages stageInfo

      val () = PackageNameVersionSet.app (PackageFinder.check fndr) pars

      (* Common post-stage operations *)

      val () = postStagePackage dir stageInfo false
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
        rev errs
      end;

fun stagePackage dir fndr repo namever chk minimal =
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

        val () = postStageTarball dir fndr stageInfo contents minimal
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

fun stageTarball dir fndr tarFile contents minimal =
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

        val () = postStageTarball dir fndr stageInfo contents minimal
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
            Print.blockProgram Print.Consistent 2
              (Print.ppString dest ::
               Print.ppString ":" ::
               List.map (Print.sequence Print.addNewline o ppSrc) srcs)
      in
        fn plan =>
           case StringMap.toList plan of
             [] => Print.skip
           | cp :: cps =>
             Print.blockProgram Print.Consistent 0
               (ppCopy cp ::
                List.map (Print.sequence Print.addNewline o ppCopy) cps)
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
            Dagify.mk
              {importer = impt,
               directory = pdir,
               theories = theories}

        val thys = Dagify.unwind thys

        val theories = Dagify.theories thys
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
  fun stageTheory dir namever pkg {directory = srcDir} =
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

          val () = postStagePackage dir stageInfo true
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

fun cleanupStaged dir namever =
    let
      val stageInfo = stagingPackageInfo dir namever
    in
      nukeStaged stageInfo
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
        rev errs
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

fun checkUpload dir repo namevers =
    let
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
              NONE => DirectoryError.AncestorNotOnRepo (anc,repo) :: errs
            | SOME chk' =>
              if Checksum.equal chk chk' then errs
              else DirectoryError.AncestorWrongChecksumOnRepo (anc,repo) :: errs
          end

      val (namevers,unknown) =
          let
            fun isKnown nv = member nv dir
          in
            PackageNameVersionSet.partition isKnown namevers
          end

      val errs = []

      val errs =
          let
            fun add (nv,acc) = DirectoryError.NotInstalled nv :: acc
          in
            PackageNameVersionSet.foldl add errs unknown
          end

      val errs =
          let
            fun check (nv,acc) =
                if not (DirectoryRepo.member nv repo) then acc
                else DirectoryError.AlreadyOnRepo (nv,repo) :: acc
          in
            PackageNameVersionSet.foldl check errs namevers
          end

      val errs =
          let
            val ancs = ancestorsSet dir namevers

            val ancs = PackageNameVersionSet.difference ancs namevers
          in
            PackageNameVersionSet.foldl checkAnc errs ancs
          end
    in
      rev errs
    end;

(***
fun upload dir repo namevers =
    let
(*OpenTheoryDebug
      val errs = checkUpload dir repo namevers

      val _ = not (DirectoryError.existsFatal errs) orelse
              raise Bug "Directory.upload: fatal error"
*)
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

      val response = DirectoryRepo.upload repo info chk
    in
      response
    end;
***)

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
