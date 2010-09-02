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
(* The package staging directory.                                            *)
(* ------------------------------------------------------------------------- *)

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

      val packages = DirectoryPackages.mk {rootDirectory = rootDir}

      val () =
          let
            val dir =
                DirectoryPath.mkStagingPackagesDirectory
                  {rootDirectory = rootDir}

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
                in
                  DirectoryRepo.mk
                    {name = name,
                     rootUrl = url,
                     rootDirectory = rootDir}
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

fun rootDirectory (Directory {rootDirectory = x, ...}) = {rootDirectory = x};

fun config (Directory {config = x, ...}) = x;

fun system dir = DirectoryConfig.system (config dir);

fun packages (Directory {packages = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Looking up repos in the package directory.                                *)
(* ------------------------------------------------------------------------- *)

fun repos (Directory {repos = x, ...}) = x;

fun peekRepo dir n =
    List.find (equal n o DirectoryRepo.name) (repos dir);

fun getRepo dir n =
    case peekRepo dir n of
      SOME r => r
    | NONE => raise Error ("no repo named " ^ n ^ " in config file");

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

fun member name dir = Option.isSome (peek dir name);

fun checksum dir name = DirectoryPackages.checksum (packages dir) name;

(* ------------------------------------------------------------------------- *)
(* Dependencies in the package directory.                                    *)
(* ------------------------------------------------------------------------- *)

fun parents dir name =
    DirectoryPackages.parents (packages dir) name;

fun children dir name =
    DirectoryPackages.children (packages dir) name;

fun ancestors dir name =
    DirectoryPackages.ancestors (packages dir) name;

fun descendents dir name =
    DirectoryPackages.descendents (packages dir) name;

fun ancestorsSet dir names =
    DirectoryPackages.ancestorsSet (packages dir) names;

fun descendentsSet dir names =
    DirectoryPackages.descendentsSet (packages dir) names;

(* ------------------------------------------------------------------------- *)
(* Generate a valid installation order.                                      *)
(* ------------------------------------------------------------------------- *)

fun installOrder dir names =
    DirectoryPackages.installOrder (packages dir) names;

(* ------------------------------------------------------------------------- *)
(* Listing packages in the package directory.                                *)
(* ------------------------------------------------------------------------- *)

fun list dir = DirectoryPackages.list (packages dir);

(* ------------------------------------------------------------------------- *)
(* Post-stage functions.                                                     *)
(* ------------------------------------------------------------------------- *)

fun postStagePackage dir stageInfo = ();

fun postStageTarball dir finder stageInfo contents =
    let
      val sys = system dir

      (* Unpack the tarball *)

      val () = PackageInfo.unpackTarball sys stageInfo

      (* Common post-stage operations *)

      val () = postStagePackage dir stageInfo
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Staging packages for installation.                                        *)
(* ------------------------------------------------------------------------- *)

fun checkStagePackage dir repo name chk =
    if member name dir then [DirectoryError.AlreadyInstalled name]
    else
      let
        val errs = []

        val errs =
            case DirectoryRepo.peek repo name of
              NONE =>
              let
                val r = DirectoryRepo.name repo
              in
                DirectoryError.NotOnRepo (name,r) :: errs
              end
            | SOME chk' =>
              if Checksum.equal chk' chk then errs
              else
                let
                  val r = DirectoryRepo.name repo
                in
                  DirectoryError.WrongChecksumOnRepo (name,r) :: errs
                end
      in
        rev errs
      end;

fun stagePackage dir finder repo name chk =
    let
(*OpenTheoryDebug
      val errs = checkStagePackage dir repo name chk

      val _ = not (DirectoryError.existsFatal errs) orelse
              raise Bug "Directory.stagePackage: fatal error"
*)
      val sys = system dir

      (* Make a package info for the stage directory *)

      val stageInfo = stagingPackageInfo dir name

      (* Create the stage directory *)

      val () = PackageInfo.createDirectory stageInfo
    in
      let
        (* Download the package tarball *)

        val () = DirectoryRepo.download sys repo stageInfo

        (* List the contents of the tarball *)

        val contents = PackageInfo.contentsTarball sys stageInfo

        (* Common post-stage operations *)

        val () = postStageTarball dir finder stageInfo contents
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
      val PackageTarball.Contents {name,...} = contents
    in
      if member name dir then [DirectoryError.AlreadyInstalled name]
      else []
    end;

fun stageTarball dir finder tarFile contents =
    let
(*OpenTheoryDebug
      val errs = checkStageTarball dir contents

      val _ = not (DirectoryError.existsFatal errs) orelse
              raise Bug "Directory.stageTarball: fatal error"
*)
      val PackageTarball.Contents {name,...} = contents

      val sys = system dir

      (* Make a package info for the stage directory *)

      val stageInfo = stagingPackageInfo dir name

      (* Create the stage directory *)

      val () = PackageInfo.createDirectory stageInfo
    in
      let
        (* Copy the package tarball *)

        val () = PackageInfo.copyTarball sys stageInfo tarFile

        (* Create the tarball checksum *)

        val () = PackageInfo.createChecksum sys stageInfo

        (* Common post-stage operations *)

        val () = postStageTarball dir finder stageInfo contents
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
  fun checkDep dir (name,errs) =
      if member name dir then errs
      else DirectoryError.UninstalledParent name :: errs;

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
                DirectoryError.FilenameClash {srcs = srcs, dest = dest} :: errs
              end
      in
        StringMap.foldl check
      end;
in
  fun checkStageTheory dir name pkg =
      let
        val errs = []

        val errs =
            if not (member name dir) then errs
            else DirectoryError.AlreadyInstalled name :: errs

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

  fun copyExtraFile sys srcDir info tag =
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

          val {cp = cmd} = DirectoryConfig.cpSystem sys

          val cmd = cmd ^ " " ^ srcFilename ^ " " ^ destFilename

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

  fun copyExtraFiles sys srcDir info pkg =
      let
        val Package.Package' {tags,theories} = Package.dest pkg

        val tags = map (copyExtraFile sys srcDir info) tags
      in
        Package.mk (Package.Package' {tags = tags, theories = theories})
      end;
in
  fun stageTheory dir name pkg {directory = srcDir} =
      let
(*OpenTheoryDebug
        val errs = checkStageTheory dir name pkg

        val _ = not (DirectoryError.existsFatal errs) orelse
                raise Bug "Directory.stageTheory: fatal error"
*)
        val sys = system dir

        (* Make a package info for the stage directory *)

        val stageInfo = stagingPackageInfo dir name

        (* Create the stage directory *)

        val () = PackageInfo.createDirectory stageInfo
      in
        let
          (* Copy the articles over *)

          val pkg = copyArticles srcDir stageInfo pkg

          (* Copy the extra files over *)

          val pkg = copyExtraFiles sys srcDir stageInfo pkg

          (* Write the new theory file *)

          val () =
              let
                val file = PackageInfo.theoryFile stageInfo

                val {filename} = PackageInfo.joinDirectory stageInfo file
              in
                Package.toTextFile {package = pkg, filename = filename}
              end

          (* Create the tarball *)

          val () = PackageInfo.createTarball sys stageInfo

          (* Create the checksum *)

          val () = PackageInfo.createChecksum sys stageInfo

          (* Common post-stage operations *)

          val () = postStagePackage dir stageInfo
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

        val () = DirectoryPackages.add pkgs pkgInfo
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
    if not (member name dir) then [DirectoryError.NotInstalled name]
    else
      let
        val errs = []

        val desc = descendents dir name

        val errs =
            if PackageNameSet.null desc then errs
            else
              let
                fun add (n,acc) = DirectoryError.InstalledDescendent n :: acc
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

      val _ = not (DirectoryError.existsFatal errs) orelse
              raise Bug "Directory.uninstall: fatal error"
*)

      val Directory {packages = pkgs, ...} = dir

      val info = packageInfo dir name

      (* Nuke the package directory *)

      val () = PackageInfo.nukeDirectory info

      (* Delete from the list of installed packages *)

      val () = DirectoryPackages.delete pkgs name
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Uploading packages from the package directory to a repo.                  *)
(* ------------------------------------------------------------------------- *)

fun checkUpload dir repo name =
    case peek dir name of
      NONE => [DirectoryError.NotInstalled name]
    | SOME info =>
      let
        fun checkAnc (anc,errs) =
            let
              val chk =
                  case checksum dir anc of
                    SOME c => c
                  | NONE =>
                    let
                      val err =
                          "depends on package " ^ PackageName.toString name ^
                          " which seems to be badly installed"
                    in
                      raise Error err
                    end
            in
              case DirectoryRepo.peek repo anc of
                NONE =>
                let
                  val r = DirectoryRepo.name repo
                in
                  DirectoryError.AncestorNotOnRepo (anc,r) :: errs
                end
              | SOME chk' =>
                if Checksum.equal chk chk' then errs
                else
                  let
                    val r = DirectoryRepo.name repo
                  in
                    DirectoryError.AncestorWrongChecksumOnRepo (anc,r) :: errs
                  end
            end

        val errs = []

        val errs =
            if not (DirectoryRepo.member name repo) then errs
            else
              let
                val r = DirectoryRepo.name repo
              in
                DirectoryError.AlreadyOnRepo (name,r) :: errs
              end

        val ancs = ancestors dir name

        val errs = PackageNameSet.foldl checkAnc errs ancs
      in
        rev errs
      end;

fun upload dir repo name =
    let
(*OpenTheoryDebug
      val errs = checkUpload dir repo name

      val _ = not (DirectoryError.existsFatal errs) orelse
              raise Bug "Directory.upload: fatal error"
*)
      val info = get dir name

      val () = DirectoryRepo.upload repo info
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* A package finder.                                                         *)
(* ------------------------------------------------------------------------- *)

fun finder dir = PackageFinder.mk (peek dir);

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
