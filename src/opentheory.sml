(* ========================================================================= *)
(* THE OPENTHEORY PROGRAM FOR PROCESSING THEORY PACKAGES                     *)
(*                                                                           *)
(* Copyright (c) 2004 Joe Hurd                                               *)
(*                                                                           *)
(* OpenTheory is free software; you can redistribute it and/or modify        *)
(* it under the terms of the GNU General Public License as published by      *)
(* the Free Software Foundation; either version 2 of the License, or         *)
(* (at your option) any later version.                                       *)
(*                                                                           *)
(* OpenTheory is distributed in the hope that it will be useful,             *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *)
(* GNU General Public License for more details.                              *)
(*                                                                           *)
(* You should have received a copy of the GNU General Public License         *)
(* along with OpenTheory; if not, write to the Free Software                 *)
(* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA *)
(* ========================================================================= *)

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val homeEnvVar = "HOME"
and rootHomeDir = ".opentheory";

(* ------------------------------------------------------------------------- *)
(* The program name and version.                                             *)
(* ------------------------------------------------------------------------- *)

val program = "opentheory";

val version = "1.0";

val versionString = program^" "^version^" (release 20100827)"^"\n";

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun annotateOptions s =
    let
      fun mk {switches,arguments,description,processor} =
          {switches = switches,
           arguments = arguments,
           description = "(" ^ s ^ ") " ^ description,
           processor = processor}
    in
      fn opts => map mk opts
    end;

(* ------------------------------------------------------------------------- *)
(* Package directory.                                                        *)
(* ------------------------------------------------------------------------- *)

val rootDirectoryOption : string option ref = ref NONE;

val rootDirectory =
    let
      val rdir : {directory : string, autoInit : bool} option ref = ref NONE
    in
      fn () =>
         case !rdir of
           SOME dir => dir
         | NONE =>
           let
             val dir =
                 case !rootDirectoryOption of
                   SOME d => {directory = d, autoInit = false}
                 | NONE =>
                   case OS.Process.getEnv homeEnvVar of
                     NONE => raise Error "please specify the package directory"
                   | SOME homeDir =>
                     let
                       val d =
                           OS.Path.joinDirFile
                             {dir = homeDir, file = rootHomeDir}
                     in
                       {directory = d, autoInit = true}
                     end

             val () = rdir := SOME dir
           in
             dir
           end
    end;

(* ------------------------------------------------------------------------- *)
(* Initializing a package directory.                                         *)
(* ------------------------------------------------------------------------- *)

fun initDirectory {rootDirectory = r} =
    let
      val () = chat ("Creating package directory " ^ r)

      val () = Directory.create {rootDirectory = r}
    in
      Directory.mk {rootDirectory = r}
    end;

(* ------------------------------------------------------------------------- *)
(* Package directory.                                                        *)
(* ------------------------------------------------------------------------- *)

val directory =
    let
      val rdir : Directory.directory option ref = ref NONE
    in
      fn () =>
         case !rdir of
           SOME dir => dir
         | NONE =>
           let
             val dir =
                 let
                   val {directory = r, autoInit} = rootDirectory ()
                 in
                   if (OS.FileSys.isDir r handle OS.SysErr _ => false) then
                     Directory.mk {rootDirectory = r}
                   else if autoInit then
                     initDirectory {rootDirectory = r}
                   else
                     raise Error ("package directory does not exist: " ^ r)
                 end

             val () = rdir := SOME dir
           in
             dir
           end
    end;

(* ------------------------------------------------------------------------- *)
(* A simple package finder.                                                  *)
(* ------------------------------------------------------------------------- *)

fun directoryFinder () = Directory.finder (directory ());

(* ------------------------------------------------------------------------- *)
(* Package repo.                                                             *)
(* ------------------------------------------------------------------------- *)

val repoOption : string list ref = ref [];

fun repository () =
    let
      val dir = directory ()

      val repos = Directory.repos dir

      val () =
          if not (null repos) then ()
          else raise Error "no repos listed in config file"
    in
      case !repoOption of
        [] => hd repos
      | [n] => Directory.getRepo dir n
      | _ :: _ :: _ => raise Error "too many repos given on command line"
    end;

fun repositories () =
    let
      val dir = directory ()

      val repos = Directory.repos dir

      val () =
          if not (null repos) then ()
          else raise Error "no repos listed in config file"

      val ns = !repoOption
    in
      if null ns then repos
      else List.map (Directory.getRepo dir) ns
    end;

(* ------------------------------------------------------------------------- *)
(* Options for displaying command help.                                      *)
(* ------------------------------------------------------------------------- *)

local
  open Useful Options;
in
  val helpOpts : opt list = [];
end;

(* ------------------------------------------------------------------------- *)
(* Options for displaying package information.                               *)
(* ------------------------------------------------------------------------- *)

datatype info =
    AncestorInfo
  | ArticleInfo
  | ChildInfo
  | DescendentInfo
  | FileInfo
  | MetaInfo
  | NameInfo
  | ParentInfo
  | SummaryInfo
  | TheoryInfo;

fun savableInfo info =
    case info of
      ArticleInfo => true
    | _ => false;

val infoOutputFilename = ref {filename = "-"};

val infoOutputList : (info * {filename : string}) list ref = ref [];

fun setInfoOutputFilename filename =
    let
      val () = infoOutputFilename := {filename = filename}
    in
      ()
    end;

fun mkInfoOutput info = (info, !infoOutputFilename);

fun addInfoOutput info =
    let
      val () = infoOutputList := !infoOutputList @ [mkInfoOutput info]
    in
      ()
    end;

local
  open Useful Options;
in
  val infoOpts : opt list =
      [{switches = ["--name"], arguments = [],
        description = "display the package name",
        processor = beginOpt endOpt (fn _ => addInfoOutput MetaInfo)},
       {switches = ["--meta"], arguments = [],
        description = "display the package meta-information",
        processor = beginOpt endOpt (fn _ => addInfoOutput MetaInfo)},
       {switches = ["--files"], arguments = [],
        description = "list the package files",
        processor = beginOpt endOpt (fn _ => addInfoOutput FileInfo)},
       {switches = ["--deps"], arguments = [],
        description = "list direct package dependencies",
        processor = beginOpt endOpt (fn _ => addInfoOutput ParentInfo)},
       {switches = ["--deps+"], arguments = [],
        description = "list all package dependencies",
        processor = beginOpt endOpt (fn _ => addInfoOutput AncestorInfo)},
       {switches = ["--uses"], arguments = [],
        description = "list direct package users",
        processor = beginOpt endOpt (fn _ => addInfoOutput ChildInfo)},
       {switches = ["--uses+"], arguments = [],
        description = "list all package users",
        processor = beginOpt endOpt (fn _ => addInfoOutput DescendentInfo)},
       {switches = ["--summary"], arguments = [],
        description = "display the package summary",
        processor = beginOpt endOpt (fn _ => addInfoOutput SummaryInfo)},
       {switches = ["--theory"], arguments = [],
        description = "display the package theory graph",
        processor = beginOpt endOpt (fn _ => addInfoOutput TheoryInfo)},
       {switches = ["--article"], arguments = [],
        description = "compile the package to an article",
        processor = beginOpt endOpt (fn _ => addInfoOutput ArticleInfo)},
       {switches = ["-o","--output"], arguments = ["FILE"],
        description = "write subsequent package information to FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => setInfoOutputFilename s)}];
end;

(* ------------------------------------------------------------------------- *)
(* Options for displaying command help.                                      *)
(* ------------------------------------------------------------------------- *)

local
  open Useful Options;
in
  val initOpts : opt list = [];
end;

(* ------------------------------------------------------------------------- *)
(* Options for uninstalling theory packages.                                 *)
(* ------------------------------------------------------------------------- *)

val autoUninstall = ref false;

local
  open Useful Options;
in
  val uninstallOpts : opt list =
      [{switches = ["--auto"], arguments = [],
        description = "also uninstall dependent packages",
        processor = beginOpt endOpt (fn _ => autoUninstall := true)}];
end;

(* ------------------------------------------------------------------------- *)
(* Options for installing theory packages.                                   *)
(* ------------------------------------------------------------------------- *)

val reinstall = ref false;

val autoInstall = ref true;

local
  open Useful Options;

  fun addSuffix s {switches,arguments,description,processor} =
      {switches = map (fn x => x ^ s) switches,
       arguments = arguments,
       description = description,
       processor = processor};
in
  val installOpts : opt list =
      [{switches = ["--reinstall"], arguments = [],
        description = "uninstall the package if it exists",
        processor = beginOpt endOpt (fn _ => reinstall := true)},
       {switches = ["--manual"], arguments = [],
        description = "do not also install required packages",
        processor = beginOpt endOpt (fn _ => autoInstall := false)},
       {switches = ["--repo"], arguments = ["REPO"],
        description = "specify the repos to install from",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => repoOption := !repoOption @ [s])}] @
      map (addSuffix "-uninstall") uninstallOpts;
end;

(* ------------------------------------------------------------------------- *)
(* Options for listing installed packages.                                   *)
(* ------------------------------------------------------------------------- *)

val dependencyOrder = ref false;

val listOutput = ref "-";

local
  open Useful Options;
in
  val listOpts : opt list =
      [{switches = ["--dep-order"], arguments = [],
        description = "list packages in dependency order",
        processor = beginOpt endOpt (fn _ => dependencyOrder := true)},
       {switches = ["-o","--output"], arguments = ["FILE"],
        description = "write the package list to FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => listOutput := s)}];
end;

(* ------------------------------------------------------------------------- *)
(* Options for updating package lists.                                       *)
(* ------------------------------------------------------------------------- *)

local
  open Useful Options;
in
  val updateOpts : opt list =
      [{switches = ["--repo"], arguments = ["REPO"],
        description = "specify the repos to update",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => repoOption := !repoOption @ [s])}];
end;

(* ------------------------------------------------------------------------- *)
(* Options for uploading packages.                                           *)
(* ------------------------------------------------------------------------- *)

local
  open Useful Options;
in
  val uploadOpts : opt list =
      [{switches = ["--repo"], arguments = ["REPO"],
        description = "specify the target repo",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => repoOption := !repoOption @ [s])}];
end;

(* ------------------------------------------------------------------------- *)
(* Commands.                                                                 *)
(* ------------------------------------------------------------------------- *)

datatype command =
    Help
  | Info
  | Init
  | Install
  | List
  | Uninstall
  | Update
  | Upload;

val allCommands = [Help,Info,Init,Install,List,Uninstall,Update,Upload];

fun commandString cmd =
    case cmd of
      Help => "help"
    | Info => "info"
    | Init => "init"
    | Install => "install"
    | List => "list"
    | Uninstall => "uninstall"
    | Update => "update"
    | Upload => "upload";

fun commandArgs cmd =
    case cmd of
      Help => ""
    | Info => " <package-name>|input.thy|input.art"
    | Init => ""
    | Install => " <package-name>|input.thy"
    | List => ""
    | Uninstall => " <package-name>"
    | Update => ""
    | Upload => " <package-name>";

fun commandDescription cmd =
    case cmd of
      Help => "display command help"
    | Info => "display package information"
    | Init => "initialize package directory"
    | Install => "install a theory package"
    | List => "list installed theory packages"
    | Uninstall => "uninstall a theory package"
    | Update => "update repo package lists"
    | Upload => "upload a theory package to a repo";

fun commandOpts cmd =
    case cmd of
      Help => helpOpts
    | Info => infoOpts
    | Init => initOpts
    | Install => installOpts
    | List => listOpts
    | Uninstall => uninstallOpts
    | Update => updateOpts
    | Upload => uploadOpts;

val allCommandStrings = map commandString allCommands;

local
  val allCommandCommandStrings =
      map (fn c => (c, commandString c)) allCommands;
in
  fun commandFromString s =
      case List.find (equal s o snd) allCommandCommandStrings of
        SOME (c,_) => SOME c
      | NONE => NONE;
end;

val allCommandOptions =
    let
      fun mk cmd = annotateOptions (commandString cmd) (commandOpts cmd)
    in
      List.concat (map mk allCommands)
    end;

(* ------------------------------------------------------------------------- *)
(* Program options.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  open Useful Options;
in
  val globalOpts : opt list =
      [{switches = ["-d","--root-dir"], arguments = ["DIR"],
        description = "the theory package directory",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => rootDirectoryOption := SOME s)}];
end;

local
  fun mkProgramOptions header opts =
      {name = program,
       version = versionString,
       header = "usage: "^program^" "^header^"\n",
       footer = "Read from stdin or write to stdout using " ^
                "the special - filename.\n",
       options = opts @ Options.basicOptions};

  val globalUsage = "[global options] command [command options] ...";

  val globalHeader =
      let
        fun f cmd =
            ["  " ^ program ^ " " ^ commandString cmd ^ " ...",
             " " ^ commandDescription cmd]

        val alignment =
            [{leftAlign = true, padChar = #"."},
             {leftAlign = true, padChar = #" "}]

        val table = alignTable alignment (map f allCommands)
      in
        globalUsage ^ "\n" ^
        "where the possible commands are:\n" ^
        join "\n" table ^ "\n"
      end;
in
  val globalOptions =
      mkProgramOptions
        (globalHeader ^ "Displaying global options:")
        globalOpts;

  fun commandOptions cmd =
      mkProgramOptions
        (commandString cmd ^ " [" ^ commandString cmd ^ " options]" ^
         commandArgs cmd ^ "\n" ^
         capitalize (commandDescription cmd) ^ ".\n" ^
         "Displaying " ^ commandString cmd ^ " options:")
        (commandOpts cmd);

  fun programOptions () =
      mkProgramOptions
        (globalHeader ^ "Displaying all options:")
        (annotateOptions "global" globalOpts @ allCommandOptions);
end;

fun exit x : unit = Options.exit (programOptions ()) x;

fun succeed () = Options.succeed (programOptions ());

fun fail mesg = Options.fail (programOptions ()) mesg;

fun usage mesg = Options.usage (programOptions ()) mesg;

fun commandUsage cmd mesg = Options.usage (commandOptions cmd) mesg;

(* ------------------------------------------------------------------------- *)
(* Input types.                                                              *)
(* ------------------------------------------------------------------------- *)

datatype input =
    ArticleInput of {filename : string}
  | PackageInput of PackageName.name
  | TarballInput of {filename : string}
  | TheoryInput of {filename : string};

fun fromStringInput cmd inp =
    case total PackageName.fromString inp of
      SOME name => PackageInput name
    | NONE =>
      let
        val filename = {filename = inp}
      in
        if Article.isFilename filename then ArticleInput filename
        else if PackageInfo.isTarball filename then TarballInput filename
        else if Package.isFilename filename then TheoryInput filename
        else commandUsage cmd ("unknown type of input: " ^ inp)
      end;

fun defaultInfoOutputList inp =
    case inp of
      ArticleInput _ => [mkInfoOutput SummaryInfo]
    | PackageInput _ => [mkInfoOutput MetaInfo]
    | TarballInput _ => [mkInfoOutput NameInfo]
    | TheoryInput _ => [mkInfoOutput SummaryInfo];

fun readInfoOutputList inp =
    let
      val l = !infoOutputList
    in
      if null l then defaultInfoOutputList inp else l
    end;

(* ------------------------------------------------------------------------- *)
(* Displaying command help.                                                  *)
(* ------------------------------------------------------------------------- *)

fun help () = usage "displaying command help";

(* ------------------------------------------------------------------------- *)
(* Displaying package information.                                           *)
(* ------------------------------------------------------------------------- *)

local
  fun getCached r f () =
      case !r of
        SOME x => x
      | NONE =>
        let
          val x = f ()

          val () = r := SOME x
        in
          x
        end;

  local
    val cache : PackageInfo.info option option ref = ref NONE;

    fun compute () = NONE;
  in
    fun setInfo info = cache := SOME (SOME info);

    val getInfo = getCached cache compute;
  end;

  local
    val cache : Package.package option option ref = ref NONE;

    fun compute () =
        case getInfo () of
          SOME info => SOME (PackageInfo.package info)
        | NONE => NONE;
  in
    fun setPackage pkg = cache := SOME (SOME pkg);

    val getPackage = getCached cache compute;
  end;

  local
    val cache : PackageName.name option option ref = ref NONE;

    fun compute () =
        case getInfo () of
          SOME info => SOME (PackageInfo.name info)
        | NONE =>
          case getPackage () of
            SOME pkg => SOME (Package.name pkg)
          | NONE => NONE;
  in
    fun setName name = cache := SOME (SOME name);

    val getName = getCached cache compute;
  end;

  local
    val cache : {directory : string} option option ref = ref NONE;

    fun compute () =
        case getInfo () of
          SOME info => SOME (PackageInfo.directory info)
        | NONE => NONE;
  in
    fun setDirectory dir = cache := SOME (SOME dir);

    val getDirectory = getCached cache compute;
  end;

  local
    val cache : {filename : string} list option option ref = ref NONE;

    fun compute () =
        case getInfo () of
          SOME info =>
          let
            val files = PackageInfo.allFiles info

            val files = map (PackageInfo.joinDirectory info) files
          in
            SOME files
          end
        | NONE => NONE;
  in
    fun setFiles files = cache := SOME (SOME files);

    val getFiles = getCached cache compute;
  end;

  local
    val cache : bool option option ref = ref NONE;

    fun compute () = NONE;
  in
    fun setSavable sav = cache := SOME (SOME sav);

    val getSavable = getCached cache compute;
  end;

  local
    val cache : (Graph.graph * Theory.theory) option option ref = ref NONE;

    fun compute () =
        case getDirectory () of
          NONE => NONE
        | SOME {directory = dir} =>
          case getPackage () of
            NONE => NONE
          | SOME pkg =>
            case getSavable () of
              NONE => NONE
            | SOME sav =>
              let
                val finder = directoryFinder ()

                val graph = Graph.empty {savable = sav}

                val imps = TheorySet.empty

                val int = Interpretation.natural

                val thy =
                    Graph.importPackage graph
                      {finder = finder,
                       directory = dir,
                       imports = imps,
                       interpretation = int,
                       package = pkg}
              in
                SOME thy
              end;
  in
    val getTheory = getCached cache compute;
  end;

  local
    val cache : Article.article option option ref = ref NONE;

    fun compute () =
        case getTheory () of
          SOME (_,thy) => SOME (Theory.article thy)
        | NONE => NONE;
  in
    fun setArticle art = cache := SOME (SOME art);

    val getArticle = getCached cache compute;
  end;

  local
    val cache : Summary.summary option option ref = ref NONE;

    fun compute () =
        case getArticle () of
          SOME art =>
          let
            val ths = Article.thms art

            val sum = Summary.fromThms ths
          in
            SOME sum
          end
        | NONE => NONE;
  in
    val getSummary = getCached cache compute;
  end;

  fun outputPackageNameSet names file =
      let
        fun mk n = PackageName.toString n ^ "\n"

        val names = PackageNameSet.toList names

        val strm = Stream.map mk (Stream.fromList names)
      in
        Stream.toTextFile file strm
      end;

  fun processInfoOutput (inf,file) =
      case inf of
        AncestorInfo =>
        let
          val dir = directory ()

          val pkg =
              case getPackage () of
                SOME p => p
              | NONE => raise Error "no package"

          val names = PackageNameSet.fromList (Package.packages pkg)

          val names = Directory.ancestorsSet dir names
        in
          outputPackageNameSet names file
        end
      | ArticleInfo =>
        let
          val art =
              case getArticle () of
                SOME a => a
              | NONE => raise Error "no article"

          val {filename} = file
        in
          Article.toTextFile {article = art, filename = filename}
        end
      | ChildInfo =>
        let
          val dir = directory ()

          val name =
              case getName () of
                SOME n => n
              | NONE => raise Error "no name"

          val names = Directory.children dir name
        in
          outputPackageNameSet names file
        end
      | DescendentInfo =>
        let
          val dir = directory ()

          val name =
              case getName () of
                SOME n => n
              | NONE => raise Error "no name"

          val names = Directory.descendents dir name
        in
          outputPackageNameSet names file
        end
      | FileInfo =>
        let
          fun mk {filename} = filename ^ "\n"

          val files =
              case getFiles () of
                SOME f => f
              | NONE => raise Error "no files"

          val strm = Stream.map mk (Stream.fromList files)
        in
          Stream.toTextFile file strm
        end
      | MetaInfo =>
        let
          val pkg =
              case getPackage () of
                SOME p => p
              | NONE => raise Error "no package"

          val tags = Package.tags pkg

          val strm = Print.toStream Tag.ppList tags
        in
          Stream.toTextFile file strm
        end
      | NameInfo =>
        let
          val name =
              case getName () of
                SOME n => n
              | NONE => raise Error "no name"

          val strm = Print.toStream PackageName.pp name
        in
          Stream.toTextFile file strm
        end
      | ParentInfo =>
        let
          val pkg =
              case getPackage () of
                SOME p => p
              | NONE => raise Error "no package"

          val names = PackageNameSet.fromList (Package.packages pkg)
        in
          outputPackageNameSet names file
        end
      | SummaryInfo =>
        let
          val sum =
              case getSummary () of
                SOME s => s
              | NONE => raise Error "no summary"

          val show =
              case getPackage () of
                SOME pkg => Show.fromTags (Package.tags pkg)
              | NONE => Show.default

          val {filename} = file
        in
          Summary.toTextFile
            {show = show,
             summary = sum,
             filename = filename}
        end
      | TheoryInfo =>
        let
          val thy =
              case getTheory () of
                SOME (_,t) => t
              | NONE => raise Error "no theory"

          val tags = []

          val theories =
              Graph.packageTheory {expand = Theory.isPackage} thy

          val package =
              Package.mk
                (Package.Package'
                   {tags = tags,
                    theories = theories})

          val {filename} = file
        in
          Package.toTextFile
            {package = package,
             filename = filename}
        end;

  fun processInfoOutputList infs =
      let
        val sav = List.exists (savableInfo o fst) infs

        val () = setSavable sav

        val () = List.app processInfoOutput infs

(*OpenTheoryDebug
        val () =
            let
              val i = ObjectRead.theInferenceCount ()

              val s = Print.toString ObjectRead.ppInferenceCount i
            in
              print ("Inference functions:\n" ^ s ^ "\n")
            end
*)
      in
        ()
      end;
in
  fun infoPackage name infs =
      let
        val dir = directory ()
      in
        case Directory.peek dir name of
          NONE => raise Error ("can't find package " ^ PackageName.toString name)
        | SOME info =>
          let
            val () = setInfo info
          in
            processInfoOutputList infs
          end
      end;

  fun infoTarball file infs =
      let
        val name =
            case PackageInfo.destTarball file of
              SOME n => n
            | NONE => raise Bug "infoTarball: bad filename"

        val () = setName name
      in
        processInfoOutputList infs
      end;

  fun infoTheory {filename} infs =
      let
        val dir = OS.Path.dir filename

        fun joinDir {filename} =
            {filename = OS.Path.concat (dir,filename)}

        val pkg = Package.fromTextFile {filename = filename}

        val files =
            {filename = filename} ::
            map joinDir (Package.articles pkg) @
            map (joinDir o Package.filenameExtraFile) (Package.extraFiles pkg)

        val () = setDirectory {directory = dir}

        val () = setPackage pkg

        val () = setFiles files
      in
        processInfoOutputList infs
      end;

  fun infoArticle {filename} infs =
      let
        val dir = OS.Path.dir filename

        val sav = List.exists (savableInfo o fst) infs

        val imp = Article.empty

        val int = Interpretation.natural

        val art =
            Article.fromTextFile
              {savable = sav,
               import = imp,
               interpretation = int,
               filename = filename}

        val files = [{filename = filename}]

        val () = setDirectory {directory = dir}

        val () = setArticle art

        val () = setFiles files
      in
        processInfoOutputList infs
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Initializing a package directory.                                         *)
(* ------------------------------------------------------------------------- *)

fun init () =
    let
      val {directory = d, autoInit = _} = rootDirectory ()

      val _ = initDirectory {rootDirectory = d}
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Uninstalling theory packages.                                             *)
(* ------------------------------------------------------------------------- *)

fun uninstallPackage dir name =
    let
      val errs = Directory.checkUninstall dir name

      val () =
          if null errs then ()
          else
            let
              val s = DirectoryError.toStringList errs
            in
              if DirectoryError.existsFatal errs then raise Error s
              else warn ("package uninstall warnings:\n" ^ s)
            end

      val () = Directory.uninstall dir name

      val () = chat ("uninstalled package " ^ PackageName.toString name)
    in
      ()
    end;

fun uninstallAuto dir name =
    let
      val desc =
          if not (!autoUninstall) then []
          else Directory.installOrder dir (Directory.descendents dir name)

      val names = rev (name :: desc)

      val () = app (uninstallPackage dir) names
    in
      ()
    end;

fun uninstall name =
    let
      val dir = directory ()

      val name = PackageName.fromString name
    in
      uninstallAuto dir name
    end
    handle Error err =>
      raise Error (err ^ "\ntheory package uninstall failed");

(* ------------------------------------------------------------------------- *)
(* Installing theory packages.                                               *)
(* ------------------------------------------------------------------------- *)

fun installAuto master name =
    case DirectoryRepo.peek master name of
      NONE =>
        let
          val err =
              "package " ^ PackageName.toString name ^
              " not found on " ^ DirectoryRepo.toString master ^ " repo"
        in
          raise Error err
        end
    | SOME chk =>
      let
        val () = installAutoFind master name chk
      in
        ()
      end

and installAutoFind master name chk =
    let
      val dir = directory ()
    in
      case Directory.checksum dir name of
        SOME chk' =>
        if Checksum.equal chk' chk then ()
        else
          let
            val err =
                "a package called " ^ PackageName.toString name ^
                " with a different checksum is already installed"
          in
            raise Error err
          end
      | NONE =>
        let
          val repos = repositories ()
        in
          case DirectoryRepo.find repos (name,chk) of
            NONE =>
            let
              val err =
                  "package " ^ PackageName.toString name ^
                  " with specific checksum not found on any repo"
            in
              raise Error err
            end
          | SOME repo => installAutoRepo master repo name chk
        end
    end

and installAutoRepo master repo name chk =
    let
      val () = chat ("installed package " ^ PackageName.toString name)
    in
      ()
    end

and installAutoFinder master =
    let
      fun finder name =
          let
            val dir = directory ()

            val () = installAuto master name
          in
            Directory.peek dir name
          end
    in
      PackageFinder.mk finder
    end;

fun installAutoFree name =
    let
      val dir = directory ()
    in
      if Directory.member name dir then ()
      else
        let
          val repos = repositories ()
        in
          case DirectoryRepo.first repos name of
            SOME (repo,chk) => installAutoRepo repo repo name chk
          | NONE =>
            let
              val err =
                  "can't find package " ^ PackageName.toString name ^
                  " in any repo"
            in
              raise Error err
            end
        end
    end;

fun installFinder master =
    if not (!autoInstall) then directoryFinder ()
    else installAutoFinder master;

fun installPackage name =
    let
      val dir = directory ()

      val repos = repositories ()
    in
      case DirectoryRepo.first repos name of
        NONE =>
        let
          val err =
              "can't find package " ^ PackageName.toString name ^
              " in any repo"
        in
          raise Error err
        end
      | SOME (repo,chk) =>
        let
          val errs = Directory.checkStagePackage dir repo name chk

          val (replace,errs) =
              if not (!reinstall) then (false,errs)
              else DirectoryError.removeAlreadyInstalled errs

          val () =
              if null errs then ()
              else
                let
                  val s = DirectoryError.toStringList errs
                in
                  if DirectoryError.existsFatal errs then raise Error s
                  else warn ("package install warnings:\n" ^ s)
                end

          val () = if replace then uninstallAuto dir name else ()

          val finder = installFinder repo

          val () = Directory.stagePackage dir finder repo name chk

          val () = Directory.installStaged dir name

          val () = chat ((if replace then "re" else "") ^ "installed package " ^
                         PackageName.toString name)
        in
          ()
        end
    end
    handle Error err =>
      raise Error (err ^ "\ntheory package install failed");

fun installTheory filename =
    let
      val dir = directory ()

      val pkg = Package.fromTextFile filename

      val name = Package.name pkg

      val srcDir =
          let
            val {filename = thyFile} = filename
          in
            {directory = OS.Path.dir thyFile}
          end

      val errs = Directory.checkStageTheory dir name pkg

      val (replace,errs) =
          if not (!reinstall) then (false,errs)
          else DirectoryError.removeAlreadyInstalled errs

      val (pars,errs) =
          if not (!autoInstall) then ([],errs)
          else DirectoryError.removeUninstalledParent errs

      val () =
          if null errs then ()
          else
            let
              val s = DirectoryError.toStringList errs
            in
              if DirectoryError.existsFatal errs then raise Error s
              else warn ("package install warnings:\n" ^ s)
            end

      val () = List.app installAutoFree pars

      val () = if replace then uninstallAuto dir name else ()

      val () = Directory.stageTheory dir name pkg srcDir

      val () = Directory.installStaged dir name

      val () = chat ((if replace then "re" else "") ^ "installed package " ^
                     PackageName.toString name)
    in
      ()
    end
    handle Error err =>
      raise Error (err ^ "\npackage install from theory file failed");

(* ------------------------------------------------------------------------- *)
(* Listing installed packages.                                               *)
(* ------------------------------------------------------------------------- *)

fun list () =
    let
      val dir = directory ()

      val pkgs = Directory.list dir

      val pkgs =
          if !dependencyOrder then Directory.installOrder dir pkgs
          else PackageNameSet.toList pkgs

      fun mk name = PackageName.toString name ^ "\n"

      val strm = Stream.map mk (Stream.fromList pkgs)

      val ref f = listOutput
    in
      Stream.toTextFile {filename = f} strm
    end;

(* ------------------------------------------------------------------------- *)
(* Update repo package lists.                                                *)
(* ------------------------------------------------------------------------- *)

fun updateRepo repo =
    let
      val () = DirectoryRepo.update repo

      val () = chat ("updated package list for " ^
                     DirectoryRepo.toString repo ^ " repo")
    in
      ()
    end
    handle Error err =>
      raise Error (err ^ "\nrepo update failed");

fun update () =
    let
      val repos = repositories ()
    in
      List.app updateRepo repos
    end;

(* ------------------------------------------------------------------------- *)
(* Upload a theory package to a repo.                                        *)
(* ------------------------------------------------------------------------- *)

fun upload name =
    let
      val dir = directory ()

      val repo = repository ()

      val name = PackageName.fromString name

      val () = updateRepo repo

      val errs = Directory.checkUpload dir repo name

      val () =
          if null errs then ()
          else
            let
              val s = DirectoryError.toStringList errs
            in
              if DirectoryError.existsFatal errs then raise Error s
              else warn ("package upload warnings:\n" ^ s)
            end

      val () = Directory.upload dir repo name

      val () = chat ("uploaded package " ^ PackageName.toString name ^
                     " to " ^ DirectoryRepo.toString repo ^ " repo")
    in
      ()
    end
    handle Error err =>
      raise Error (err ^ "\ntheory package upload failed");

(* ------------------------------------------------------------------------- *)
(* Top level.                                                                *)
(* ------------------------------------------------------------------------- *)

val () =
let
  val work = CommandLine.arguments ();

  val (_,work) = Options.processOptions globalOptions work

  val (cmd,work) =
      case work of
        [] => usage "no command specified"
      | s :: work =>
        case commandFromString s of
          SOME cmd => (cmd,work)
        | NONE => usage ("bad command specified: \"" ^ s ^ "\"")

  val (_,work) = Options.processOptions (commandOptions cmd) work

  val () =
      case (cmd,work) of
        (Help,[]) => help ()
      | (Info,[inp]) =>
        let
          val inp = fromStringInput cmd inp

          val infs = readInfoOutputList inp
        in
          case inp of
            ArticleInput file => infoArticle file infs
          | PackageInput name => infoPackage name infs
          | TarballInput file => infoTarball file infs
          | TheoryInput file => infoTheory file infs
        end
      | (Init,[]) => init ()
      | (Install,[inp]) =>
        let
          val inp = fromStringInput cmd inp
        in
          case inp of
            ArticleInput _ => commandUsage cmd "cannot install an article"
          | PackageInput name => installPackage name
          | TarballInput _ => raise Bug "not implemented"
          | TheoryInput file => installTheory file
        end
      | (List,[]) => list ()
      | (Uninstall,[pkg]) => uninstall pkg
      | (Update,[]) => update ()
      | (Upload,[pkg]) => upload pkg
      | _ =>
        commandUsage cmd ("bad arguments for " ^ commandString cmd ^ " command")
in
  succeed ()
end
handle Error s => die (program^" failed:\n" ^ s)
     | Bug s => die ("BUG found in "^program^" program:\n" ^ s);
