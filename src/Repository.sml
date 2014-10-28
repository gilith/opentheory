(* ========================================================================= *)
(* PACKAGE REPOSITORIES                                                      *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure Repository :> Repository =
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
(* Clean up the remote repository package lists.                             *)
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
  fun checkRemotesDirectory cfgs {directory = dir} =
      let
        val dirStrm = OS.FileSys.openDir dir

        fun readAll dels utds =
            case OS.FileSys.readDir dirStrm of
              NONE => (dels,utds)
            | SOME file =>
              let
                val name =
                    case RepositoryChecksums.destFilename {filename = file} of
                      SOME n => n
                    | NONE =>
                      raise Error ("bad filename "^file^" in repos directory")

                val filename =
                    {filename = OS.Path.joinDirFile {dir = dir, file = file}}
              in
                case RepositoryConfig.findRemote cfgs name of
                  NONE => readAll ((name,filename) :: dels) utds
                | SOME cfg =>
                  let
                    val age = ageFilename filename

                    val threshold = RepositoryConfig.refreshRemote cfg

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
(* Creating a new package repository.                                        *)
(* ------------------------------------------------------------------------- *)

local
  fun createNewRepository rootDir cfg =
      let
        val () = createDirectory {directory = rootDir}

        val () =
            let
              val dir =
                  RepositoryPath.mkPackagesDirectory
                    {rootDirectory = rootDir}
            in
              createDirectory dir
            end

        val () =
            let
              val dir =
                  RepositoryPath.mkStagedPackagesDirectory
                    {rootDirectory = rootDir}
            in
              createDirectory dir
            end

        val () =
            let
              val dir =
                  RepositoryPath.mkRemoteRepositoriesDirectory
                    {rootDirectory = rootDir}
            in
              createDirectory dir
            end

        val () =
            let
              val {filename = file} =
                  RepositoryPath.mkConfigFilename
                    {rootDirectory = rootDir}
            in
              RepositoryConfig.toTextFile {config = cfg, filename = file}
            end

        val () =
            let
              val file =
                  RepositoryPath.mkInstalledFilename
                    {rootDirectory = rootDir}
            in
              RepositoryChecksums.create file
            end
      in
        ()
      end
      handle OS.SysErr (s,_) => raise Error ("system error: " ^ s);
in
  fun create {rootDirectory = rootDir, config = cfg} =
      createNewRepository rootDir cfg
      handle Error err =>
        let
          val err =
              "couldn't create a new package repository in directory\n  " ^
              rootDir ^ "\n" ^ err
        in
          raise Error err
        end;
end;

(* ------------------------------------------------------------------------- *)
(* A type of package repositories.                                           *)
(* ------------------------------------------------------------------------- *)

datatype repository =
    Repository of
      {rootDirectory : string,
       config : RepositoryConfig.config,
       packages : RepositoryPackages.packages,
       remotes : RepositoryRemote.remote list};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

local
  fun initialize rootDir =
      let
        val config =
            let
              val filename =
                  RepositoryPath.mkConfigFilename {rootDirectory = rootDir}
            in
              RepositoryConfig.fromTextFile filename
            end

        val packages =
            let
              val sys = RepositoryConfig.system config
            in
              RepositoryPackages.mk
                {system = sys,
                 rootDirectory = rootDir}
            end

        val remotes =
            let
              val sys = RepositoryConfig.system config
              and cfgs = RepositoryConfig.remotes config

              val dir =
                  RepositoryPath.mkRemoteRepositoriesDirectory
                    {rootDirectory = rootDir}

              val utds = checkRemotesDirectory cfgs dir

              fun mkRemote cfg =
                  let
                    val name = RepositoryConfig.nameRemote cfg
                    and {url} = RepositoryConfig.urlRemote cfg
                  in
                    RepositoryRemote.mk
                      {system = sys,
                       name = name,
                       rootUrl = url,
                       rootDirectory = rootDir,
                       upToDate = List.exists (PackageName.equal name) utds}
                  end
            in
              List.map mkRemote cfgs
            end
      in
        Repository
          {rootDirectory = rootDir,
           config = config,
           packages = packages,
           remotes = remotes}
      end
      handle OS.SysErr (s,_) => raise Error ("system error: " ^ s);
in
  fun mk {rootDirectory = rootDir} =
      initialize rootDir
      handle Error err =>
        let
          val err =
              "couldn't open a package repository in directory\n  " ^
              rootDir ^ "\n" ^ err
        in
          raise Error err
        end;
end;

fun rootDirectory (Repository {rootDirectory = x, ...}) = {rootDirectory = x};

fun config (Repository {config = x, ...}) = x;

fun authors dir = RepositoryConfig.authors (config dir);

fun system dir = RepositoryConfig.system (config dir);

fun packages (Repository {packages = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Connected remote repositories.                                            *)
(* ------------------------------------------------------------------------- *)

fun remotes (Repository {remotes = x, ...}) = x;

fun peekRemote repo n =
    let
      fun pred remote = PackageName.equal (RepositoryRemote.name remote) n
    in
      List.find pred (remotes repo)
    end;

fun getRemote repo n =
    case peekRemote repo n of
      SOME r => r
    | NONE =>
      let
        val err = "no repo named " ^ PackageName.toString n ^ " in config file"
      in
        raise Error err
      end;

(* ------------------------------------------------------------------------- *)
(* Acceptable licenses.                                                      *)
(* ------------------------------------------------------------------------- *)

fun licenses repo = RepositoryConfig.licenses (config repo);

fun peekLicense repo n = RepositoryConfig.findLicense (licenses repo) n;

fun knownLicense repo n = Option.isSome (peekLicense repo n);

fun getLicense repo n =
    case peekLicense repo n of
      SOME l => l
    | NONE =>
      let
        val {name} = n

        val err = "no license named " ^ name ^ " in config file"
      in
        raise Error err
      end;

(* ------------------------------------------------------------------------- *)
(* Repository paths.                                                         *)
(* ------------------------------------------------------------------------- *)

fun configFilename repo =
    RepositoryPath.mkConfigFilename (rootDirectory repo);

fun installedFilename repo =
    RepositoryPath.mkInstalledFilename (rootDirectory repo);

fun packagesDirectory repo =
    RepositoryPath.mkPackagesDirectory (rootDirectory repo);

fun packageDirectory repo name =
    RepositoryPath.mkPackageDirectory (rootDirectory repo) name;

fun stagedPackagesDirectory repo =
    RepositoryPath.mkStagedPackagesDirectory (rootDirectory repo);

fun stagedPackageDirectory repo name =
    RepositoryPath.mkStagedPackageDirectory (rootDirectory repo) name;

fun remoteRepositoriesDirectory repo =
    RepositoryPath.mkRemoteRepositoriesDirectory (rootDirectory repo);

fun remoteRepositoryChecksumsFilename repo remote =
    RepositoryPath.mkRemoteRepositoryChecksumsFilename
      (rootDirectory repo) remote;

(* ------------------------------------------------------------------------- *)
(* Repository packages.                                                      *)
(* ------------------------------------------------------------------------- *)

fun mkPackage repo namever checksum =
    let
      val sys = system repo
      and {directory} = packageDirectory repo namever
    in
      Package.mk
        {system = sys,
         nameVersion = namever,
         checksum = checksum,
         directory = directory}
    end;

fun mkStagedPackage repo namever checksum =
    let
      val sys = system repo
      and {directory} = stagedPackageDirectory repo namever
    in
      Package.mk
        {system = sys,
         nameVersion = namever,
         checksum = checksum,
         directory = directory}
    end;

(* ------------------------------------------------------------------------- *)
(* Installed package sets.                                                   *)
(* ------------------------------------------------------------------------- *)

fun all repo = RepositoryPackages.all (packages repo);

fun latest repo = RepositoryPackages.latest (packages repo);

(* ------------------------------------------------------------------------- *)
(* Looking up installed packages by name.                                    *)
(* ------------------------------------------------------------------------- *)

fun peek repo = RepositoryPackages.peek (packages repo);

fun get repo namever =
    case peek repo namever of
      SOME pkg => pkg
    | NONE => raise Error "Repository.get";

fun member namever repo = Option.isSome (peek repo namever);

(* ------------------------------------------------------------------------- *)
(* Package versions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun nameVersions repo =
    RepositoryPackages.nameVersions (packages repo);

fun latestNameVersion repo =
    RepositoryPackages.latestNameVersion (packages repo);

fun isLatestNameVersion repo =
    RepositoryPackages.isLatestNameVersion (packages repo);

fun getLatestNameVersion repo =
    RepositoryPackages.getLatestNameVersion (packages repo);

fun warnLatestNameVersion repo =
    RepositoryPackages.warnLatestNameVersion (packages repo);

fun warnLatestNameVersionList repo =
    RepositoryPackages.warnLatestNameVersionList (packages repo);

fun previousNameVersion repo =
    RepositoryPackages.previousNameVersion (packages repo);

(* ------------------------------------------------------------------------- *)
(* Package author.                                                           *)
(* ------------------------------------------------------------------------- *)

fun author repo = RepositoryPackages.author (packages repo);

fun knownAuthor repo = RepositoryPackages.knownAuthor (packages repo);

fun selfAuthor repo =
    let
      val self = PackageAuthorSet.fromList (authors repo)
    in
      knownAuthor repo self
    end;

(* ------------------------------------------------------------------------- *)
(* Package theory graph.                                                     *)
(* ------------------------------------------------------------------------- *)

fun emptyTheories repo = RepositoryPackages.emptyTheories (packages repo);

(* ------------------------------------------------------------------------- *)
(* Package requirements.                                                     *)
(* ------------------------------------------------------------------------- *)

fun requiresInstalled repo =
    RepositoryPackages.requiresInstalled (packages repo);

fun requiredBy repo =
    RepositoryPackages.requiredBy (packages repo);

fun isRequired repo =
    RepositoryPackages.isRequired (packages repo);

fun requires repo =
    RepositoryPackages.requires (packages repo);

fun requiresNameVersions repo =
    RepositoryPackages.requiresNameVersions (packages repo);

fun requiresPackages repo =
    RepositoryPackages.requiresPackages (packages repo);

fun requiresTheorems repo =
    RepositoryPackages.requiresTheorems (packages repo);

(* ------------------------------------------------------------------------- *)
(* Included packages.                                                        *)
(* ------------------------------------------------------------------------- *)

fun includes repo =
    RepositoryPackages.includes (packages repo);

fun includedBy repo =
    RepositoryPackages.includedBy (packages repo);

fun isIncluded repo =
    RepositoryPackages.isIncluded (packages repo);

fun includesRTC repo =
    RepositoryPackages.includesRTC (packages repo);

fun includedByRTC repo =
    RepositoryPackages.includedByRTC (packages repo);

(* ------------------------------------------------------------------------- *)
(* Subtheory packages.                                                       *)
(* ------------------------------------------------------------------------- *)

fun subtheoriesInstalled repo =
    RepositoryPackages.subtheoriesInstalled (packages repo);

fun subtheoryOf repo =
    RepositoryPackages.subtheoryOf (packages repo);

fun isSubtheory repo =
    RepositoryPackages.isSubtheory (packages repo);

fun subtheoryOfRTC repo =
    RepositoryPackages.subtheoryOfRTC (packages repo);

fun subtheories repo =
    RepositoryPackages.subtheories (packages repo);

fun subtheoriesRTC repo =
    RepositoryPackages.subtheoriesRTC (packages repo);

(* ------------------------------------------------------------------------- *)
(* Arranging packages in dependency order.                                   *)
(* ------------------------------------------------------------------------- *)

fun includeOrder repo =
    RepositoryPackages.includeOrder (packages repo);

fun includeOrdered repo =
    RepositoryPackages.includeOrdered (packages repo);

fun dependencyOrder repo =
    RepositoryPackages.dependencyOrder (packages repo);

fun dependencyOrdered repo =
    RepositoryPackages.dependencyOrdered (packages repo);

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun closedDependencies repo =
    RepositoryPackages.closedDependencies (packages repo);

fun acyclicDependencies repo =
    RepositoryPackages.acyclicDependencies (packages repo);

fun upToDateDependencies repo =
    RepositoryPackages.upToDateDependencies (packages repo);

(* ------------------------------------------------------------------------- *)
(* Upgrading theory source files.                                            *)
(* ------------------------------------------------------------------------- *)

fun upgradeTheory repo =
    let
      fun checksum namever =
          case peek repo namever of
            SOME pkg => SOME (Package.checksum pkg)
          | NONE => raise Bug "Repository.upgradeTheory"

      fun latest namever _ =
          let
            val PackageNameVersion.NameVersion' {name,version} =
                PackageNameVersion.dest namever
          in
            case warnLatestNameVersion repo name of
              NONE => NONE
            | SOME nv =>
              if PackageNameVersion.equalVersion version nv then NONE
              else SOME (nv, checksum nv)
          end
    in
      PackageInformation.updateIncludes latest
    end;

(* ------------------------------------------------------------------------- *)
(* Package finders.                                                          *)
(* ------------------------------------------------------------------------- *)

fun finder repo = RepositoryPackages.finder (packages repo);

fun stagedFinder repo =
    let
      fun stagedPeek namever chko =
          let
            val pkg = mkStagedPackage repo namever NONE
          in
            if not (Package.existsDirectory pkg) then NONE else
            let
              val match =
                  case chko of
                    SOME chk => Checksum.equal chk (Package.checksum pkg)
                  | NONE => true
            in
              if match then SOME pkg else NONE
            end
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
          val err = RepositoryError.TagError (name,"is missing")
        in
          (RepositoryError.add errs err, NONE)
        end
      | [v] => (errs, SOME v)
      | _ :: _ :: _ =>
        let
          val err = RepositoryError.TagError (name,"is declared multiple times")
        in
          (RepositoryError.add errs err, NONE)
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

              val err = RepositoryError.TagError (name,msg)
            in
              RepositoryError.add errs err
            end
          | SOME n =>
            case namever of
              NONE => errs
            | SOME nv =>
              if PackageName.equal (PackageNameVersion.name nv) n then errs
              else
                let
                  val msg = "does not match"

                  val err = RepositoryError.TagError (name,msg)
                in
                  RepositoryError.add errs err
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

              val err = RepositoryError.TagError (name,msg)
            in
              RepositoryError.add errs err
            end
          | SOME v =>
            case namever of
              NONE => errs
            | SOME nv =>
              if PackageVersion.equal (PackageNameVersion.version nv) v then
                errs
              else
                let
                  val msg = "does not match"

                  val err = RepositoryError.TagError (name,msg)
                in
                  RepositoryError.add errs err
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

              val err = RepositoryError.TagError (name,msg)
            in
              RepositoryError.add errs err
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

              val err = RepositoryError.TagError (name,msg)
            in
              RepositoryError.add errs err
            end
      end;

  fun checkLicenseTag repo tags errs =
      let
        val name = PackageName.licenseTag

        val (errs,so) = getTag name tags errs
      in
        case so of
          NONE => errs
        | SOME s =>
          if knownLicense repo {name = s} then errs
          else
            let
              val msg = s ^ " is not acceptable"

              val err = RepositoryError.TagError (name,msg)
            in
              RepositoryError.add errs err
            end
      end;
in
  fun checkTags repo namever tags =
      let
        val errs = RepositoryError.clean

        val errs = checkNameTag namever tags errs

        val errs = checkVersionTag namever tags errs

        val errs = checkDescriptionTag tags errs

        val errs = checkAuthorTag tags errs

        val errs = checkLicenseTag repo tags errs
      in
        errs
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Summarizing packages.                                                     *)
(* ------------------------------------------------------------------------- *)

fun theoryGraph fndr pkg =
    let
      val graph = TheoryGraph.empty {savable = false}
      and imps = TheorySet.empty
      and int = Interpretation.natural
    in
      TheoryGraph.importPackage fndr graph
        {imports = imps,
         interpretation = int,
         package = pkg}
    end;

fun summary fndr pkg =
    let
      val (_,thy) = theoryGraph fndr pkg
    in
      TheoryGraph.summary thy
    end;

(* ------------------------------------------------------------------------- *)
(* Common operations staging packages for installation.                      *)
(* ------------------------------------------------------------------------- *)

fun postStagePackage repo fndr pkg warnSummary {tool} =
    let
      val namever = Package.nameVersion pkg
      and info = Package.information pkg

      (* Check the package tags *)

      val () =
          let
            val tags = PackageInformation.tags info

            val errs = checkTags repo (SOME namever) tags
          in
            if not (RepositoryError.fatal errs) then ()
            else raise Error (RepositoryError.report errs)
          end

      (* Check the package summary *)

      val sum = summary fndr pkg

      val () =
          if not warnSummary then ()
          else
            let
              val name = PackageNameVersion.name namever
              and reqs = Package.requires pkg

              val ctxt =
                  case requiresTheorems repo reqs of
                    NONE => Summary.NoContext
                  | SOME ths => PackageTheorems.packageContext sum ths

              val chkThms = not (PackageName.isExport name)

              val chks = {checkTheorems = chkThms}
            in
              PackageSummary.check chks ctxt (Package.show pkg) sum
            end

      (* Create the package theorems *)

      val () =
          let
            val seqs = PackageSummary.provides sum

            val thms = PackageTheorems.mk namever seqs
          in
            Package.writeTheorems pkg thms
          end

      (* Create the package document *)

      val () =
          let
            val chk = Package.checksum pkg

            val files =
                let
                  val {filename = theory} = Package.theoryFile pkg
                  and {filename = tarball} = Package.tarballFile pkg
                in
                  {theory = SOME theory, tarball = SOME tarball}
                end

            val doc =
                PackageDocument.mk
                  (PackageDocument.Document'
                     {information = SOME info,
                      checksum = SOME chk,
                      summary = sum,
                      files = files,
                      tool = tool})
          in
            Package.writeDocument pkg doc
          end
    in
      ()
    end;

fun postStageTarball repo fndr pkg tool =
    let
      val minimal =
          let
            val cfg = RepositoryConfig.install (config repo)
          in
            {minimal = RepositoryConfig.minimalInstall cfg}
          end

      (* Unpack the tarball *)

      val () = Package.unpackTarball pkg minimal

      (* Check the included packages are installed *)

      val () =
          let
            fun check (nv,chk) = PackageFinder.check fndr nv chk

            val incs = Package.includes pkg
          in
            List.app check incs
          end

      (* Common post-stage operations *)

      val () = postStagePackage repo fndr pkg false tool
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Staging packages for installation.                                        *)
(* ------------------------------------------------------------------------- *)

fun checkStagePackage repo remote namever chk =
    if member namever repo then
      RepositoryError.fromList [RepositoryError.AlreadyInstalled namever]
    else
      let
        val errs = RepositoryError.clean

        val errs =
            let
              val pkg = mkStagedPackage repo namever NONE
            in
              if not (Package.existsDirectory pkg) then errs
              else
                RepositoryError.add errs
                  (RepositoryError.AlreadyStaged namever)
            end

        val errs =
            case RepositoryRemote.peek remote namever of
              NONE =>
              RepositoryError.add errs
                (RepositoryError.NotOnRemote (namever,remote))
            | SOME chk' =>
              if Checksum.equal chk' chk then errs
              else
                RepositoryError.add errs
                  (RepositoryError.WrongChecksumOnRemote (namever,remote))
      in
        errs
      end;

fun stagePackage repo fndr remote namever chk tool =
    let
(*OpenTheoryDebug
      val errs = checkStagePackage repo remote namever chk

      val () =
          if not (RepositoryError.fatal errs) then ()
          else raise Bug "Repository.stagePackage: fatal error"
*)
      (* Make a staged package *)

      val pkg = mkStagedPackage repo namever NONE

      (* Create the staging directory *)

      val () = Package.createDirectory pkg
    in
      let
        (* Download the package tarball *)

        val () = RepositoryRemote.download remote pkg

        (* Common post-stage operations *)

        val () = postStageTarball repo fndr pkg tool
      in
        ()
      end
      handle e =>
        let
          val () = Package.nukeDirectory pkg
        in
          raise e
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Staging tarballs for installation.                                        *)
(* ------------------------------------------------------------------------- *)

fun checkStageTarball repo tar =
    let
      val namever = PackageTarball.nameVersion tar
    in
      if member namever repo then
        RepositoryError.fromList [RepositoryError.AlreadyInstalled namever]
      else
        let
          val errs = RepositoryError.clean

          val errs =
              let
                val pkg = mkStagedPackage repo namever NONE
              in
                if not (Package.existsDirectory pkg) then errs
                else
                  RepositoryError.add errs
                    (RepositoryError.AlreadyStaged namever)
              end
        in
          errs
        end
    end;

fun stageTarball repo fndr tar tool =
    let
(*OpenTheoryDebug
      val errs = checkStageTarball repo tar

      val () =
          if not (RepositoryError.fatal errs) then ()
          else raise Bug "Repository.stageTarball: fatal error"
*)
      val namever = PackageTarball.nameVersion tar

      (* Make a staged package *)

      val pkg = mkStagedPackage repo namever NONE

      (* Create the staging directory *)

      val () = Package.createDirectory pkg
    in
      let
        (* Copy the package tarball *)

        val () = Package.copyTarball pkg tar

        (* Common post-stage operations *)

        val () = postStageTarball repo fndr pkg tool
      in
        ()
      end
      handle e =>
        let
          val () = Package.nukeDirectory pkg
        in
          raise e
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Staging theory files for installation.                                    *)
(* ------------------------------------------------------------------------- *)

local
  datatype fileCopyPlan =
      FileCopyPlan of
        {name : string, filename : string option} list StringMap.map;

  val emptyFileCopyPlan = FileCopyPlan (StringMap.new ());

  fun addFileCopyPlan (src, {filename = dest}) (FileCopyPlan plan) =
      let
        val hits = Option.getOpt (StringMap.peek plan dest, [])

        val hits = src :: hits

        val plan = StringMap.insert plan (dest,hits)
      in
        FileCopyPlan plan
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
        fn FileCopyPlan plan =>
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
                RepositoryError.add errs
                  (RepositoryError.FilenameClash {srcs = srcs, dest = dest})
              end
      in
        fn errs => fn FileCopyPlan plan => StringMap.foldl check errs plan
      end;

  fun mkFileCopyPlan info pkg =
      let
        fun addReserved ((name,filename),plan) =
            let
              val src = {name = name, filename = NONE}
              and dest = filename
            in
              addFileCopyPlan (src,dest) plan
            end

        fun addArticle ({filename},plan) =
            let
              val src = {name = "article file", filename = SOME filename}

              val dest = Article.normalizeFilename {filename = filename}
            in
              addFileCopyPlan (src,dest) plan
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
              addFileCopyPlan (src,dest) plan
            end

        val reserved =
            [("theory file", Package.theoryFile pkg),
             ("tarball", Package.tarballFile pkg),
             ("theorems", Package.theoremsFile pkg),
             ("document", Package.documentFile pkg)]

        val plan = emptyFileCopyPlan

        val plan = List.foldl addReserved plan reserved

        val plan =
            List.foldl addArticle plan (PackageInformation.articleFiles info)

        val plan =
            List.foldl addExtra plan (PackageInformation.extraFiles info)
      in
        plan
      end;

  fun checkRequires repo (name,errs) =
      if Option.isSome (latestNameVersion repo name) then errs
      else
        RepositoryError.add errs
          (RepositoryError.NoVersionInstalled name);

  fun checkInclude repo ((namever,chk),errs) =
      case peek repo namever of
        NONE =>
        RepositoryError.add errs
          (RepositoryError.UninstalledInclude (namever,chk))
      | SOME pkg =>
        case chk of
          NONE => errs
        | SOME c =>
          if Checksum.equal c (Package.checksum pkg) then errs
          else
            RepositoryError.add errs
              (RepositoryError.WrongChecksumInclude namever);
in
  fun checkStageTheory repo namever info =
      let
        val errs = checkTags repo namever (PackageInformation.tags info)
      in
        if RepositoryError.fatal errs then errs
        else
          let
            val namever = PackageInformation.nameVersion info

            val errs =
                if not (member namever repo) then errs
                else
                  RepositoryError.add errs
                    (RepositoryError.AlreadyInstalled namever)

            val errs =
                let
                  val pkg = mkStagedPackage repo namever NONE
                in
                  if not (Package.existsDirectory pkg) then errs
                  else
                    RepositoryError.add errs
                      (RepositoryError.AlreadyStaged namever)
                end

            val errs =
                let
                  val reqs = PackageInformation.requires info
                in
                  List.foldl (checkRequires repo) errs reqs
                end

            val errs =
                let
                  val incs = PackageInformation.includes info
                in
                  List.foldl (checkInclude repo) errs incs
                end

            val errs =
                let
                  val pkg = mkPackage repo namever NONE

                  val plan = mkFileCopyPlan info pkg

(*OpenTheoryTrace1
                  val () =
                      Print.trace ppFileCopyPlan
                        "Repository.checkStageTheory: plan" plan
*)
                in
                  checkFileCopyPlan errs plan
                end
          in
            errs
          end
      end;
end;

local
  fun copyArticle srcDir pkg thy =
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
                  "Repository.stageTheory.copyArticle: srcFilename"
                  srcFilename
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
                Package.joinDirectory pkg {filename = pkgFilename}

(*OpenTheoryTrace1
            val () =
                Print.trace Print.ppString
                  "Repository.stageTheory.copyArticle: destFilename"
                  destFilename
*)
            val () =
                Article.toTextFile
                  {article = art,
                   version = ArticleVersion.install,
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

  fun copyExtraFile sys srcDir pkg tag =
      case PackageTag.toExtra tag of
        NONE => tag
      | SOME extra =>
        let
          val {filename = srcFilename} = PackageExtra.filename extra

(*OpenTheoryTrace1
          val () =
              Print.trace Print.ppString
                "Repository.stageTheory.copyExtraFile: srcFilename"
                srcFilename
*)
          val srcFilename = OS.Path.concat (srcDir,srcFilename)

          val extra = PackageExtra.normalize extra

          val {filename = pkgFilename} = PackageExtra.filename extra

          val {filename = destFilename} =
              Package.joinDirectory pkg {filename = pkgFilename}

          val {cp} = RepositorySystem.cp sys

          val cmd = cp ^ " " ^ srcFilename ^ " " ^ destFilename

(*OpenTheoryTrace1
          val () = trace (cmd ^ "\n")
*)
          val () =
              if OS.Process.isSuccess (OS.Process.system cmd) then ()
              else raise Error "copying extra file failed"
        in
          PackageTag.fromExtra extra
        end;

  fun copyArticles srcDir pkg info =
      let
        val PackageInformation.Information' {tags,theories} =
            PackageInformation.dest info

        val theories = List.map (copyArticle srcDir pkg) theories
      in
        PackageInformation.mk
          (PackageInformation.Information'
             {tags = tags, theories = theories})
      end;

  fun copyExtraFiles sys srcDir pkg info =
      let
        val PackageInformation.Information' {tags,theories} =
            PackageInformation.dest info

        val tags = List.map (copyExtraFile sys srcDir pkg) tags
      in
        PackageInformation.mk
          (PackageInformation.Information'
             {tags = tags, theories = theories})
      end;

  fun checkTheory fndr pkg info =
      let
(*OpenTheoryTrace1
        val () = trace "Repository.stageTheory.checkTheory\n"
*)
        val PackageInformation.Information' {tags,theories} =
            PackageInformation.dest info

        val {directory} = Package.directory pkg

        val graph =
            PackageTheoryGraph.mk
              {finder = fndr,
               directory = directory,
               theories = theories}

        val graph = PackageTheoryGraph.unwind graph

        val theories = PackageTheoryGraph.theories graph
      in
        PackageInformation.mk
          (PackageInformation.Information'
             {tags = tags, theories = theories})
      end;

  fun writeTheoryFile pkg info =
      let
(*OpenTheoryTrace1
        val () = trace "Repository.stageTheory.writeTheoryFile\n"
*)
        val {filename} = Package.joinDirectory pkg (Package.theoryFile pkg)
      in
        PackageInformation.toTextFile
          {information = info, filename = filename}
      end;
in
  fun stageTheory repo fndr namever info {directory = srcDir} tool =
      let
(*OpenTheoryDebug
        val errs = checkStageTheory repo (SOME namever) info

        val () =
            if not (RepositoryError.fatal errs) then ()
            else raise Bug "Repository.stageTheory: fatal error"
*)
        val sys = system repo

        (* Make a staged package *)

        val pkg = mkStagedPackage repo namever NONE

        (* Create the staging directory *)

        val () = Package.createDirectory pkg
      in
        let
          (* Copy the articles over *)

          val info = copyArticles srcDir pkg info

          (* Copy the extra files over *)

          val info = copyExtraFiles sys srcDir pkg info

          (* Check the package theory *)

          val info = checkTheory fndr pkg info

          (* Write the new theory file *)

          val () = writeTheoryFile pkg info

          (* Create the tarball *)

          val () = Package.packTarball pkg

          (* Common post-stage operations *)

          val () = postStagePackage repo fndr pkg true tool
        in
          Package.checksum pkg
        end
        handle e =>
          let
            val () = Package.nukeDirectory pkg
          in
            raise e
          end
      end
end;

(* ------------------------------------------------------------------------- *)
(* Installing staged packages into the repository.                           *)
(* ------------------------------------------------------------------------- *)

fun checkInstallStaged repo namever =
    let
      val stage = mkStagedPackage repo namever NONE
      and pkg = mkPackage repo namever NONE

      val errs = RepositoryError.clean

      val errs =
          if Package.existsDirectory stage then errs
          else
            RepositoryError.add errs
              (RepositoryError.NotStaged namever)

      val errs =
          if not (Package.existsDirectory pkg) then errs
          else
            RepositoryError.add errs
              (RepositoryError.AlreadyInstalled namever)

      val errs =
          if not (RepositoryError.isClean errs) then errs
          else
            let
              val fndr = finder repo

              fun add ((nv,co),acc) =
                  if PackageFinder.member fndr nv co then acc
                  else
                    RepositoryError.add acc
                      (RepositoryError.UninstalledInclude (nv,co))
            in
              List.foldl add errs (Package.includes stage)
            end
    in
      errs
    end;

fun installStaged repo namever chk =
    let
(*OpenTheoryDebug
      val errs = checkInstallStaged repo namever

      val () =
          if not (RepositoryError.fatal errs) then ()
          else raise Bug "Repository.installStaged: fatal error"
*)
      val stage = mkStagedPackage repo namever NONE
      and pkg = mkPackage repo namever (SOME chk)
    in
      let
        (* Rename staged package directory to package directory *)

        val {directory = stageDir} = Package.directory stage
        and {directory = pkgDir} = Package.directory pkg

        val () = renameDirectory {src = stageDir, dest = pkgDir}

        (* Update the list of installed packages *)

        val () = RepositoryPackages.add (packages repo) pkg
      in
        ()
      end
      handle e =>
        let
          val () = Package.nukeDirectory stage
        in
          raise e
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Cleaning up staged packages.                                              *)
(* ------------------------------------------------------------------------- *)

fun listStaged repo {maxAge} =
    let
      val {directory = dir} = stagedPackagesDirectory repo

      val dirStrm = OS.FileSys.openDir dir

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
                          OS.Path.joinDirFile {dir = dir, file = file}

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

fun cleanupStaged repo namever =
    let
      val stage = mkStagedPackage repo namever NONE

      val () = Package.nukeDirectory stage
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Uninstalling packages from the repository.                                *)
(* ------------------------------------------------------------------------- *)

fun checkUninstall repo namevers =
    let
      val errs = RepositoryError.clean

      val (namevers,unknown) =
          let
            fun known nv = member nv repo
          in
            PackageNameVersionSet.partition known namevers
          end

      val errs =
          let
            fun add (nv,acc) =
                RepositoryError.add acc
                  (RepositoryError.NotInstalled nv)
          in
            PackageNameVersionSet.foldl add errs unknown
          end

      val users =
          let
            val incs = includedByRTC repo namevers
          in
            PackageNameVersionSet.difference incs namevers
          end

      val errs =
          let
            fun add (nv,acc) =
                RepositoryError.add acc
                  (RepositoryError.InstalledUser nv)
          in
            PackageNameVersionSet.foldl add errs users
          end
    in
      errs
    end;

local
  fun check repo namever =
      checkUninstall repo (PackageNameVersionSet.singleton namever);
in
  fun uninstall repo namever =
      let
(*OpenTheoryDebug
        val errs = check repo namever

        val () =
            if not (RepositoryError.fatal errs) then ()
            else raise Bug "Repository.uninstall: fatal error"
*)
        (* Delete from the list of installed packages *)

        val () = RepositoryPackages.delete (packages repo) namever

        (* Nuke the package directory *)

        val () = Package.nukeDirectory (mkPackage repo namever NONE)
      in
        ()
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Comparing a package with a remote repository.                             *)
(* ------------------------------------------------------------------------- *)

fun identicalOnRemote repo =
    RepositoryPackages.identicalOnRemote (packages repo);

fun consistentWithRemote repo =
    RepositoryPackages.consistentWithRemote (packages repo);

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun pp repo =
    let
      val {rootDirectory = rootDir} = rootDirectory repo
    in
      Print.ppBracket "{" "}" Print.ppString rootDir
    end;

end
