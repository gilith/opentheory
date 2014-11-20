(* ========================================================================= *)
(* THE OPENTHEORY PROGRAM FOR PROCESSING THEORY PACKAGES                     *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val homeEnvVar = "HOME"
and opentheoryEnvVar = "OPENTHEORY"
and rootHomeDir = ".opentheory";

(* ------------------------------------------------------------------------- *)
(* The program name and version.                                             *)
(* ------------------------------------------------------------------------- *)

val program = "opentheory";

val version = "1.3";

val release = " (release 20141119)";

val homepage = "http://www.gilith.com/software/opentheory"

val versionString = program^" "^version^release^"\n";

val versionHtml =
    let
      val programLink =
          let
            val attrs = Html.singletonAttrs ("href",homepage)
          in
            Html.Anchor (attrs, [Html.Text program])
          end
    in
      [programLink, Html.Text (" " ^ version ^ release)]
    end;

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
      fn opts => List.map mk opts
    end;

(* ------------------------------------------------------------------------- *)
(* Basic format descriptions.                                                *)
(* ------------------------------------------------------------------------- *)

val describeDirFormat =
    "DIR is a directory on the file system";

val describeFileFormat =
    "FILE is a filename; use - to read from stdin or write to stdout";

val describeRepoFormat =
    "REPO is the name of a repo in the config file (e.g., gilith)";

val describeNameFormat =
    "NAME is a package name (e.g., base)";

val describeVersionFormat =
    "VERSION is a package version (e.g., 1.0)";

val describeListQueryFormat =
    "QUERY is a package query (e.g., UpToDate, Upgradable or Obsolete)";

val describeUninstallQueryFormat =
    "QUERY is a package query (e.g., NAME-VERSION or Obsolete)";

val describeUpgradeQueryFormat =
    "QUERY is a package query (e.g., NAME or Upgradable)";

val describeQueryFormat =
    "QUERY represents a subset S of the installed packages P, as follows:\n" ^
    "  1. A FUNCTION expression in the grammar below is parsed from the command\n" ^
    "     line, which represents a function f of type S -> S\n" ^
    "  2. Another function g of type S -> S is computed, which may be represented\n" ^
    "     by the FUNCTION expression ~Empty (Latest - Subtheories) All\n" ^
    "  3. The set f(g({})) is evaluated as the result, where {} is the empty set\n" ^
    "FUNCTION          // represents a function with type S -> S\n" ^
    "  <- SET          // the constant function with return value SET\n" ^
    "  || PREDICATE    // the filter function with predicate PREDICATE\n" ^
    "  || FUNCTION FUNCTION\n" ^
    "                  // \\f g s. f (g s)\n" ^
    "  || FUNCTION | FUNCTION\n" ^
    "                  // \\f g s. { p in P | p in f(s) \\/ p in g(s) }\n" ^
    "  || FUNCTION & FUNCTION\n" ^
    "                  // \\f g s. { p in P | p in f(s) /\\ p in g(s) }\n" ^
    "  || FUNCTION - FUNCTION\n" ^
    "                  // \\f g s. { p in P | p in f(s) /\\ ~p in g(s) }\n" ^
    "  || FUNCTION?    // \\f. Identity | f\n" ^
    "  || FUNCTION*    // \\f. Identity | f | f f | f f f | ...\n" ^
    "  || FUNCTION+    // \\f. f | f f | f f f | ...\n" ^
    "  || Identity     // \\s. s\n" ^
    "  || Requires     // \\s. { p in P | ?q in s. q requires p }\n" ^
    "  || RequiredBy   // \\s. { p in P | ?q in s. p requires q }\n" ^
    "  || Includes     // \\s. { p in P | ?q in s. q includes p }\n" ^
    "  || IncludedBy   // \\s. { p in P | ?q in s. p includes q }\n" ^
    "  || Subtheories  // \\s. { p in P | ?q in s. p is a subtheory of q }\n" ^
    "  || SubtheoryOf  // \\s. { p in P | ?q in s. q is a subtheory of p }\n" ^
    "  || Versions     // \\s. { p in P | ?q in s. p is a version of q }\n" ^
    "  || Latest       // \\s. { p in s | ~?q in s. q is a later version of p }\n" ^
    "  || Deprecated   // (Identity - Latest) (Requires | Includes)*\n" ^
    "  || Obsolete     // All - (Requires | Includes)*\n" ^
    "  || Upgradable   // EarlierThanRepo\n" ^
    "  || Uploadable   // Mine /\\ ~OnRepo /\\ ~EarlierThanRepo /\\ ConsistentWithRepo\n" ^
    "PREDICATE         // represents a predicate with type P -> bool\n" ^
    "  <- PREDICATE \\/ PREDICATE\n" ^
    "                  // \\f g p. f(p) \\/ g(p)\n" ^
    "  || PREDICATE /\\ PREDICATE\n" ^
    "                  // \\f g p. f(p) /\\ g(p)\n" ^
    "  || ~PREDICATE   // \\f p. ~f(p)\n" ^
    "  || Empty        // does the package have an empty theory (i.e., main { })?\n" ^
    "  || Mine         // does the package author match a name in the config file?\n" ^
    "  || Closed       // are all the required theories installed?\n" ^
    "  || Acyclic      // is the required theory graph free of cycles?\n" ^
    "  || UpToDate     // are all assumptions satisfied and inputs grounded?\n" ^
    "  || OnRepo       // is there a package with the same name on the repo?\n" ^
    "  || IdenticalOnRepo\n" ^
    "                  // is this exact same package on the repo?\n" ^
    "  || ConsistentWithRepo\n" ^
    "                  // are all the included packages consistent with the repo?\n" ^
    "  || EarlierThanRepo\n" ^
    "                  // is there a later version of this package on the repo?\n" ^
    "  || LaterThanRepo\n" ^
    "                  // is this package later than all versions on the repo?\n" ^
    "SET               // represents a set with type S\n" ^
    "  <- All          // P\n" ^
    "  || None         // {}\n" ^
    "  || NAME         // Latest { p in P | p has name NAME }\n" ^
    "  || NAME-VERSION // { p in P | p has name NAME and version VERSION }\n";

(* ------------------------------------------------------------------------- *)
(* Input types.                                                              *)
(* ------------------------------------------------------------------------- *)

datatype input =
    ArticleInput of {filename : string}
  | PackageInput of PackageNameVersion.nameVersion
  | PackageNameInput of PackageName.name
  | PackageQueryInput of RepositoryQuery.function
  | StagedPackageInput of PackageNameVersion.nameVersion
  | TarballInput of {filename : string}
  | TheoryInput of {filename : string};

fun fromStringInput inp =
    case total (destPrefix "article:") inp of
      SOME f => ArticleInput {filename = f}
    | NONE =>
      case total (destPrefix "tarball:") inp of
        SOME f => TarballInput {filename = f}
      | NONE =>
        case total (destPrefix "theory:") inp of
          SOME f => TheoryInput {filename = f}
        | NONE =>
          case total (destPrefix "staged:") inp of
            SOME nv =>
            (case total PackageNameVersion.fromString nv of
               SOME namever => StagedPackageInput namever
             | NONE => raise Error ("bad staged package name: " ^ inp))
          | NONE =>
            case total PackageNameVersion.fromString inp of
              SOME namever => PackageInput namever
            | NONE =>
              case total PackageName.fromString inp of
                SOME name => PackageNameInput name
              | NONE =>
                case total RepositoryQuery.fromString inp of
                  SOME query => PackageQueryInput query
                | NONE =>
                  let
                    val f = {filename = inp}
                  in
                    if Article.isFilename f then ArticleInput f
                    else if PackageTarball.isFilename f then TarballInput f
                    else if PackageInformation.isFilename f then TheoryInput f
                    else raise Error ("unknown type of input: " ^ inp)
                  end;

val describeInfoInputFormat =
    "INPUT is one of the following:\n" ^
    "  - A package: NAME-VERSION or NAME (for the latest version)\n" ^
    "  - A theory source file: FILE.thy or theory:FILE\n" ^
    "  - A proof article file: FILE.art or article:FILE\n" ^
    "  - A package tarball: FILE.tgz or tarball:FILE\n" ^
    "  - A package staged for installation: staged:NAME-VERSION";

val describeInputFormat =
    describeInfoInputFormat ^ "\n" ^
    "  - A subset of the installed packages: QUERY";

(* ------------------------------------------------------------------------- *)
(* Output format for basic package information.                              *)
(* ------------------------------------------------------------------------- *)

datatype infoItem =
    ChecksumItem
  | DescriptionItem
  | EmptyItem
  | NameItem
  | SeparatorItem of string
  | VersionItem;

datatype infoFormat = InfoFormat of infoItem list;

fun emptyToString empty = if empty then "T" else "F";

local
  fun getSep acc l =
      case l of
        SeparatorItem s :: l => getSep (s :: acc) l
      | _ => (String.concat (List.rev acc), l);

  fun compress l =
      let
        val (sl,l) = getSep [] l

        val l = compress' l
      in
        if sl = "" then l else SeparatorItem sl :: l
      end

  and compress' l =
      case l of
        [] => []
      | s :: l => s :: compress l;
in
  fun compressInfoFormat (InfoFormat l) = InfoFormat (compress l);
end;

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val checksumKeywordParser = exactString "CHECKSUM"
  and descriptionKeywordParser = exactString "DESCRIPTION"
  and emptyKeywordParser = exactString "EMPTY"
  and nameKeywordParser = exactString "NAME"
  and versionKeywordParser = exactString "VERSION";

  val itemParser =
      (checksumKeywordParser >> K ChecksumItem) ||
      (descriptionKeywordParser >> K DescriptionItem) ||
      (emptyKeywordParser >> K EmptyItem) ||
      (nameKeywordParser >> K NameItem) ||
      (versionKeywordParser >> K VersionItem) ||
      any >> (fn c => SeparatorItem (str c));

  val itemListParser = many itemParser;
in
  val parserInfoFormat = itemListParser >> (compressInfoFormat o InfoFormat);
end;

val describeInfoFormat =
    "FORMAT is a string containing " ^
    "{NAME,VERSION,DESCRIPTION,CHECKSUM,EMPTY}";

fun fromStringInfoFormat fmt =
    Parse.fromString parserInfoFormat fmt
    handle Parse.NoParse =>
      let
        val err =
            "bad package information format:\n  \"" ^ fmt ^ "\"\n" ^
            "correct " ^ describeInfoFormat
      in
        raise Error err
      end;

local
  fun mkItem repo namever item =
      case item of
        ChecksumItem =>
        let
          val chk =
              case Repository.peek repo namever of
                SOME pkg => Package.checksum pkg
              | NONE => raise Error "corrupt installation"
        in
          Checksum.toString chk
        end
      | DescriptionItem =>
        let
          val info =
              case Repository.peek repo namever of
                SOME pkg => Package.information pkg
              | NONE => raise Error "corrupt installation"

          val {description} = PackageInformation.description info
        in
          description
        end
      | EmptyItem =>
        let
          val empty =
              case Repository.peek repo namever of
                SOME pkg => Package.emptyTheories pkg
              | NONE => raise Error "corrupt installation"
        in
          emptyToString empty
        end
      | NameItem =>
        let
          val name = PackageNameVersion.name namever
        in
          PackageName.toString name
        end
      | SeparatorItem s => s
      | VersionItem =>
        let
          val version = PackageNameVersion.version namever
        in
          PackageVersion.toString version
        end;
in
  fun packageToStringInfoFormat repo fmt namever =
      let
        val InfoFormat items = fmt
      in
        String.concat (List.map (mkItem repo namever) items)
      end
      handle Error err =>
        let
          val err =
              "package " ^ PackageNameVersion.toString namever ^ ": " ^ err
        in
          raise Error err
        end;
end;

(* ------------------------------------------------------------------------- *)
(* Clean up a staged package.                                                *)
(* ------------------------------------------------------------------------- *)

fun cleanupStagedPackage repo nv =
    let
      val () = Repository.cleanupStaged repo nv

      val mesg =
          "cleaned up staged package " ^ PackageNameVersion.toString nv

      val () = chat mesg
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Root directory of the local package repository.                           *)
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
                   case OS.Process.getEnv opentheoryEnvVar of
                     SOME d => {directory = d, autoInit = false}
                   | NONE =>
                     case OS.Process.getEnv homeEnvVar of
                       NONE =>
                       raise Error "please specify the package directory"
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
(* Initializing a package repository.                                        *)
(* ------------------------------------------------------------------------- *)

val remoteInit = ref false;

fun initRepository {rootDirectory = r} =
    let
      val c =
          if !remoteInit then RepositoryConfig.remoteDefault
          else RepositoryConfig.default

      val () = Repository.create {rootDirectory = r, config = c}
    in
      Repository.mk {rootDirectory = r}
    end;

(* ------------------------------------------------------------------------- *)
(* The local package repository.                                             *)
(* ------------------------------------------------------------------------- *)

val repository =
    let
      fun existsDirectory d =
          OS.FileSys.isDir d
          handle OS.SysErr _ => false

      val rrepo : Repository.repository option ref = ref NONE
    in
      fn () =>
         case !rrepo of
           SOME repo => repo
         | NONE =>
           let
             val repo =
                 let
                   val {directory = r, autoInit} = rootDirectory ()
                 in
                   if existsDirectory r then
                     let
                       val repo = Repository.mk {rootDirectory = r}

                       val () =
                           let
                             val cfg = Repository.config repo

                             val cfg = RepositoryConfig.cleanup cfg
                           in
                             case RepositoryConfig.autoCleanup cfg of
                               NONE => ()
                             | SOME t =>
                               let
                                 val maxAge = {maxAge = SOME t}

                                 val nvs = Repository.listStaged repo maxAge

                                 val () =
                                     PackageNameVersionSet.app
                                       (cleanupStagedPackage repo) nvs
                               in
                                 ()
                               end
                           end
                     in
                       repo
                     end
                   else if autoInit then
                     let
                       val x = initRepository {rootDirectory = r}

                       val msg =
                           "auto-initialized package repo " ^
                           Print.toString Repository.pp x

                       val () = chat msg
                     in
                       x
                     end
                   else
                     raise Error ("package repo does not exist: " ^ r)
                 end

             val () = rrepo := SOME repo
           in
             repo
           end
    end;

fun config () = Repository.config (repository ());

fun system () = RepositoryConfig.system (config ());

(* ------------------------------------------------------------------------- *)
(* Remote repositories.                                                      *)
(* ------------------------------------------------------------------------- *)

local
  val remoteOption : RepositoryRemote.name list ref = ref [];
in
  fun addRemote s =
      let
        val n = PackageName.fromString s

        val () = remoteOption := !remoteOption @ [n]
      in
        ()
      end;

  fun remote () =
      let
        val repo = repository ()

        val remotes = Repository.remotes repo

        val () =
            if not (List.null remotes) then ()
            else raise Error "no repos listed in config file"
      in
        case !remoteOption of
          [] => hd remotes
        | [n] => Repository.getRemote repo n
        | _ :: _ :: _ => raise Error "multiple repos given on command line"
      end;

  fun remotes () =
      let
        val repo = repository ()

        val remotes = Repository.remotes repo

        val ns = !remoteOption
      in
        if List.null ns then remotes
        else List.map (Repository.getRemote repo) ns
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Finding packages in the local repository.                                 *)
(* ------------------------------------------------------------------------- *)

fun finder () = Repository.finder (repository ());

fun stagedFinder () = Repository.stagedFinder (repository ());

fun possiblyStagedFinder () =
    PackageFinder.orelsef (finder ()) (stagedFinder ());

fun latestVersion name =
    let
      val repo = repository ()
    in
      Repository.latestNameVersion repo name
    end;

fun getLatestVersion name =
    let
      val repo = repository ()
    in
      Repository.getLatestNameVersion repo name
    end;

fun previousVersion namever =
    let
      val repo = repository ()
    in
      Repository.previousNameVersion repo namever
    end;

fun evaluateQuery query =
    let
      val repo = repository ()

      val rems = remotes ()
    in
      RepositoryQuery.evaluate repo rems query
    end;

(* ------------------------------------------------------------------------- *)
(* Finding packages on remote repositories.                                  *)
(* ------------------------------------------------------------------------- *)

fun latestVersionRemotes name chko =
    let
      val rems = remotes ()
    in
      RepositoryRemote.latestNameVersionList rems name chko
    end;

fun firstRemote namever chko =
    let
      val rems = remotes ()
    in
      case chko of
        NONE =>
        (case RepositoryRemote.first rems namever of
           SOME rc => rc
         | NONE =>
           let
             val err =
                 "can't find package " ^
                 PackageNameVersion.toString namever ^
                 " in any repo"
           in
             raise Error err
           end)
      | SOME chk =>
        (case RepositoryRemote.find rems (namever,chk) of
           SOME rem => (rem,chk)
         | NONE =>
           let
             val err =
                 "can't find package " ^
                 PackageNameVersion.toString namever ^
                 " with specified checksum in any repo"
           in
             raise Error err
           end)
    end;

(* ------------------------------------------------------------------------- *)
(* Options for cleaning up staged packages.                                  *)
(* ------------------------------------------------------------------------- *)

local
  open Useful Options;
in
  val cleanupOpts : opt list = [];
end;

val cleanupFooter =
    describeNameFormat ^ ".\n" ^
    describeVersionFormat ^ ".\n" ^
    "Given no arguments this command will clean up all staged packages.\n";

(* ------------------------------------------------------------------------- *)
(* Options for exporting installed packages.                                 *)
(* ------------------------------------------------------------------------- *)

local
  open Useful Options;
in
  val exportOpts : opt list = [];
end;

val exportFooter =
    describeNameFormat ^ ".\n" ^
    describeVersionFormat ^ ".\n" ^
    "Given a NAME input this command will export the latest installed version.\n";

(* ------------------------------------------------------------------------- *)
(* Options for displaying command help.                                      *)
(* ------------------------------------------------------------------------- *)

local
  open Useful Options;
in
  val helpOpts : opt list = [];
end;

val helpFooter = "";

(* ------------------------------------------------------------------------- *)
(* Options for displaying package information.                               *)
(* ------------------------------------------------------------------------- *)

datatype info =
    ArticleInfo of ArticleVersion.version option
  | AssumptionsInfo
  | DocumentInfo
  | FilesInfo
  | FormatInfo of infoFormat
  | IncludesInfo
  | InferenceInfo
  | RequiresInfo
  | SummaryInfo
  | SymbolsInfo
  | TagsInfo
  | TheoremsInfo
  | TheoryInfo;

fun savableInfo info =
    case info of
      ArticleInfo _ => true
    | SymbolsInfo => true
    | _ => false;

datatype packageInfo =
    PackageInfo of string * info * {filename : string} option;

fun mkInfoOutput flag info = PackageInfo (flag,info,NONE);

local
  fun mkDefaultInfoOutput info = [mkInfoOutput "default" info];
in
  fun defaultInfoOutputList inp =
      let
        val info =
            case inp of
              ArticleInput _ => SummaryInfo
            | PackageInput _ => TagsInfo
            | PackageNameInput _ => TagsInfo
            | PackageQueryInput _ => TagsInfo
            | StagedPackageInput _ => TagsInfo
            | TarballInput _ => FilesInfo
            | TheoryInput _ => SummaryInfo
      in
        mkDefaultInfoOutput info
      end;
end;

val outputListInfo : packageInfo list ref = ref [];

val upgradeTheoryInfo = ref false;

val preserveTheoryInfo = ref false;

val showAssumptionsInfo = ref false;

val showDerivationsInfo = ref false;

fun infoSummaryGrammar () =
    let
      val Summary.Grammar
            {assumptionGrammar,
             axiomGrammar,
             theoremGrammar,
             ppTypeOp,
             ppConst,
             showTheoremAssumptions = _} = Summary.defaultGrammar

      val showTheoremAssumptions = !showDerivationsInfo
    in
      Summary.Grammar
        {assumptionGrammar = assumptionGrammar,
         axiomGrammar = axiomGrammar,
         theoremGrammar = theoremGrammar,
         ppTypeOp = ppTypeOp,
         ppConst = ppConst,
         showTheoremAssumptions = showTheoremAssumptions}
    end;

fun addInfoOutput flag info =
    let
      val ref l = outputListInfo

      val () = outputListInfo := mkInfoOutput flag info :: l
    in
      ()
    end;

fun setInfoOutputFilename flag filename =
    let
      val ref l = outputListInfo

      val l =
          case l of
            [] =>
            raise Error ("no package information specified before " ^
                         flag ^ " argument")
          | PackageInfo (x,i,f) :: l =>
            case f of
              SOME {filename = f} =>
              let
                val err =
                    "multiple " ^ flag ^ " arguments:\n" ^
                    "  " ^ f ^ " and\n  " ^ filename
              in
                raise Error err
              end
            | NONE => PackageInfo (x, i, SOME {filename = filename}) :: l

      val () = outputListInfo := l
    in
      ()
    end;

fun setInfoOutputVersion flag version =
    let
      val ref l = outputListInfo

      val l =
          case l of
            [] =>
            raise Error ("no package information specified before " ^
                         flag ^ " argument")
          | PackageInfo (x,i,f) :: l =>
            case i of
              ArticleInfo vo =>
              (case vo of
                 SOME v =>
                 let
                   val err =
                       "multiple " ^ flag ^ " arguments: " ^
                       ArticleVersion.toString v ^ " and " ^ version
                 in
                   raise Error err
                 end
               | NONE =>
                 let
                   val v = ArticleVersion.fromString version
                 in
                   PackageInfo (x, ArticleInfo (SOME v), f) :: l
                 end)
            | _ =>
              let
                val err =
                    "cannot specify output version for " ^ x ^
                    " package information"
              in
                raise Error err
              end

      val () = outputListInfo := l
    in
      ()
    end;

local
  fun readList inp =
      let
        val l = List.rev (!outputListInfo)
      in
        if List.null l then defaultInfoOutputList inp else l
      end;

  val defaultInfoOutputFilename = {filename = "-"};

  fun defaultize (PackageInfo (_,i,f)) =
      (i, Option.getOpt (f,defaultInfoOutputFilename));
in
  fun readInfoOutputList inp = List.map defaultize (readList inp);
end;

local
  open Useful Options;
in
  val infoOpts : opt list =
      [{switches = ["--format"], arguments = ["FORMAT"],
        description = "format package information",
        processor =
          beginOpt (stringOpt endOpt)
            (fn f => fn s =>
              addInfoOutput f (FormatInfo (fromStringInfoFormat s)))},
       {switches = ["--information"], arguments = [],
        description = "display all package information",
        processor = beginOpt endOpt (fn f => addInfoOutput f TagsInfo)},
       {switches = ["--theory"], arguments = [],
        description = "display the package theory",
        processor = beginOpt endOpt (fn f => addInfoOutput f SummaryInfo)},
       {switches = ["--article"], arguments = [],
        description = "output the package theory in article format",
        processor =
          beginOpt endOpt
            (fn f => addInfoOutput f (ArticleInfo NONE))},
       {switches = ["--requires"], arguments = [],
        description = "list satisfying required packages",
        processor = beginOpt endOpt (fn f => addInfoOutput f RequiresInfo)},
       {switches = ["--inference"], arguments = [],
        description = "display count of inference rules",
        processor = beginOpt endOpt (fn f => addInfoOutput f InferenceInfo)},
       {switches = ["--files"], arguments = [],
        description = "list package files",
        processor = beginOpt endOpt (fn f => addInfoOutput f FilesInfo)},
       {switches = ["--document"], arguments = [],
        description = "output package document in HTML format",
        processor = beginOpt endOpt (fn f => addInfoOutput f DocumentInfo)},
       {switches = ["--theory-source"], arguments = [],
        description = "output package theory source",
        processor = beginOpt endOpt (fn f => addInfoOutput f TheoryInfo)},
       {switches = ["--theorems"], arguments = [],
        description = "output package theorems in article format",
        processor = beginOpt endOpt (fn f => addInfoOutput f TheoremsInfo)},
       {switches = ["--assumptions"], arguments = [],
        description = "output package assumptions in article format",
        processor = beginOpt endOpt (fn f => addInfoOutput f AssumptionsInfo)},
       {switches = ["--symbols"], arguments = [],
        description = "list all symbols in the package",
        processor = beginOpt endOpt (fn f => addInfoOutput f SymbolsInfo)},
       {switches = ["--includes"], arguments = [],
        description = "list included packages",
        processor = beginOpt endOpt (fn f => addInfoOutput f IncludesInfo)},
       {switches = ["-o","--output"], arguments = ["FILE"],
        description = "write previous information to FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn f => fn s => setInfoOutputFilename f s)},
       {switches = ["--output-version"], arguments = ["N"],
        description = "set previous information output version",
        processor =
          beginOpt (stringOpt endOpt)
            (fn f => fn s => setInfoOutputVersion f s)},
       {switches = ["--show-assumptions"], arguments = [],
        description = "do not hide satisfied assumptions",
        processor = beginOpt endOpt (fn _ => showAssumptionsInfo := true)},
       {switches = ["--show-derivations"], arguments = [],
        description = "show assumptions and axioms for each theorem",
        processor = beginOpt endOpt (fn _ => showDerivationsInfo := true)},
       {switches = ["--upgrade-theory"], arguments = [],
        description = "upgrade theory source to latest versions",
        processor = beginOpt endOpt (fn _ => upgradeTheoryInfo := true)},
       {switches = ["--preserve-theory"], arguments = [],
        description = "do not optimize theory source",
        processor = beginOpt endOpt (fn _ => preserveTheoryInfo := true)}];
end;

val infoFooter =
    describeInfoInputFormat ^ "\n" ^
    describeNameFormat ^ ".\n" ^
    describeVersionFormat ^ ".\n" ^
    describeFileFormat ^ ".\n" ^
    describeInfoFormat ^ ".\n";

(* ------------------------------------------------------------------------- *)
(* Options for displaying command help.                                      *)
(* ------------------------------------------------------------------------- *)

local
  open Useful Options;
in
  val initOpts : opt list =
      [{switches = ["--remote"], arguments = [],
        description = "configure new package repo for remote use",
        processor =
          beginOpt endOpt
            (fn _ => remoteInit := true)}];
end;

val initFooter = "";

(* ------------------------------------------------------------------------- *)
(* Options for uninstalling packages.                                        *)
(* ------------------------------------------------------------------------- *)

val autoUninstall = ref false;

local
  open Useful Options;
in
  val uninstallOpts : opt list =
      [{switches = ["--auto"], arguments = [],
        description = "also uninstall including packages",
        processor = beginOpt endOpt (fn _ => autoUninstall := true)}];
end;

val uninstallFooter =
    describeUninstallQueryFormat ^ ".\n" ^
    describeNameFormat ^ ".\n" ^
    describeVersionFormat ^ ".\n";

(* ------------------------------------------------------------------------- *)
(* Options for installing packages.                                          *)
(* ------------------------------------------------------------------------- *)

val reinstall = ref false;

val autoInstall = ref true;

val nameInstall : PackageNameVersion.nameVersion option ref = ref NONE;

val checksumInstall : Checksum.checksum option ref = ref NONE;

val stageInstall = ref false;

local
  open Useful Options;

  fun addSuffix s {switches,arguments,description,processor} =
      {switches = List.map (fn x => x ^ s) switches,
       arguments = arguments,
       description = description,
       processor = processor};
in
  val installOpts : opt list =
      [{switches = ["--reinstall"], arguments = [],
        description = "uninstall package if it already exists",
        processor = beginOpt endOpt (fn _ => reinstall := true)}] @
      List.map (addSuffix "-uninstall") uninstallOpts @
      [{switches = ["--manual"], arguments = [],
        description = "do not also install included packages",
        processor = beginOpt endOpt (fn _ => autoInstall := false)},
       {switches = ["--name"], arguments = ["NAME-VERSION"],
        description = "confirm package name",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s =>
                nameInstall := SOME (PackageNameVersion.fromString s))},
       {switches = ["--checksum"], arguments = ["CHECKSUM"],
        description = "confirm package checksum",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => checksumInstall := SOME (Checksum.fromString s))},
       {switches = ["--stage"], arguments = [],
        description = "stage package for installation",
        processor = beginOpt endOpt (fn _ => stageInstall := true)}];
end;

val installFooter =
    describeNameFormat ^ ".\n" ^
    describeVersionFormat ^ ".\n" ^
    "Given a NAME input this command will install the latest available version.\n";

(* ------------------------------------------------------------------------- *)
(* Options for listing installed packages.                                   *)
(* ------------------------------------------------------------------------- *)

datatype orderList =
    AlphabeticalList
  | DependencyList
  | IncludeList
  | ReverseList of orderList;

local
  val refOrderList = ref AlphabeticalList;
in
  fun setOrderList ord = refOrderList := ord;

  fun reverseOrderList () = refOrderList := ReverseList (!refOrderList);

  fun orderList () = !refOrderList;
end;

local
  val refFormatList : infoFormat option ref = ref NONE;

  val defaultFormatList = InfoFormat [NameItem, SeparatorItem "-", VersionItem];
in
  fun getFormatList () = Option.getOpt (!refFormatList, defaultFormatList);

  fun setFormatList fmt =
      let
        val () = refFormatList := SOME fmt
      in
        ()
      end;
end;

val outputList = ref "-";

local
  open Useful Options;
in
  val listOpts : opt list =
      [{switches = ["--dependency-order"], arguments = [],
        description = "list packages in dependency order",
        processor = beginOpt endOpt (fn _ => setOrderList DependencyList)},
       {switches = ["--include-order"], arguments = [],
        description = "list packages in include order",
        processor = beginOpt endOpt (fn _ => setOrderList IncludeList)},
       {switches = ["--reverse-order"], arguments = [],
        description = "reverse the order",
        processor = beginOpt endOpt (fn _ => reverseOrderList ())},
       {switches = ["--format"], arguments = ["FORMAT"],
        description = "set output format",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => setFormatList (fromStringInfoFormat s))}];
end;

val listFooter =
    describeListQueryFormat ^ ".\n" ^
    describeInfoFormat ^ ".\n" ^
    "If the QUERY argument is missing the latest installed packages are listed.\n";

(* ------------------------------------------------------------------------- *)
(* Options for updating remote repository package lists.                     *)
(* ------------------------------------------------------------------------- *)

local
  open Useful Options;
in
  val updateOpts : opt list =
      [];
end;

val updateFooter = "";

(* ------------------------------------------------------------------------- *)
(* Options for upgrading installed packages with later versions on a remote  *)
(* repository.                                                               *)
(* ------------------------------------------------------------------------- *)

local
  open Useful Options;
in
  val upgradeOpts : opt list =
      [];
end;

val upgradeFooter =
    describeUpgradeQueryFormat ^ ".\n" ^
    describeNameFormat ^ ".\n" ^
    "If the QUERY argument is missing all installed packages are upgraded.\n";

(* ------------------------------------------------------------------------- *)
(* Options for uploading installed packages to a remote repository.          *)
(* ------------------------------------------------------------------------- *)

datatype setUpload =
    ManualUpload
  | SubtheoryUpload;

val setUpload = ref SubtheoryUpload;

val confirmUpload = ref true;

local
  open Useful Options;
in
  val uploadOpts : opt list =
      [{switches = ["--manual"], arguments = [],
        description = "do not also upload subtheory packages",
        processor = beginOpt endOpt (fn _ => setUpload := ManualUpload)},
       {switches = ["--yes"], arguments = [],
        description = "do not ask for confirmation",
        processor = beginOpt endOpt (fn _ => confirmUpload := false)}];
end;

val uploadFooter =
    describeNameFormat ^ ".\n" ^
    describeVersionFormat ^ ".\n" ^
    "Given NAME inputs this command will upload the latest installed versions.\n";

(* ------------------------------------------------------------------------- *)
(* Commands.                                                                 *)
(* ------------------------------------------------------------------------- *)

datatype command =
    Cleanup
  | Export
  | Help
  | Info
  | Init
  | Install
  | List
  | Uninstall
  | Update
  | Upgrade
  | Upload;

val allCommands =
    [Cleanup,
     Export,
     Help,
     Info,
     Init,
     Install,
     List,
     Uninstall,
     Update,
     Upgrade,
     Upload];

fun commandString cmd =
    case cmd of
      Cleanup => "cleanup"
    | Export => "export"
    | Help => "help"
    | Info => "info"
    | Init => "init"
    | Install => "install"
    | List => "list"
    | Uninstall => "uninstall"
    | Update => "update"
    | Upgrade => "upgrade"
    | Upload => "upload";

fun commandArgs cmd =
    case cmd of
      Cleanup => " staged:NAME-VERSION ..."
    | Export => " NAME|NAME-VERSION"
    | Help => ""
    | Info => " INPUT"
    | Init => ""
    | Install => " NAME|NAME-VERSION|FILE.thy"
    | List => " QUERY"
    | Uninstall => " QUERY"
    | Update => ""
    | Upgrade => " QUERY"
    | Upload => " NAME|NAME-VERSION ...";

fun commandDescription cmd =
    case cmd of
      Cleanup => "clean up packages staged for installation"
    | Export => "export an installed package"
    | Help => "display help on all available commands"
    | Info => "extract information from packages and files"
    | Init => "initialize a new package repo"
    | Install => "install a package from a theory file or repo"
    | List => "list installed packages"
    | Uninstall => "uninstall packages"
    | Update => "update repo package lists"
    | Upgrade => "upgrade packages with later versions on a repo"
    | Upload => "upload installed packages to a repo";

fun commandFooter cmd =
    case cmd of
      Cleanup => cleanupFooter
    | Export => exportFooter
    | Help => helpFooter
    | Info => infoFooter
    | Init => initFooter
    | Install => installFooter
    | List => listFooter
    | Uninstall => uninstallFooter
    | Update => updateFooter
    | Upgrade => upgradeFooter
    | Upload => uploadFooter;

fun commandOpts cmd =
    case cmd of
      Cleanup => cleanupOpts
    | Export => exportOpts
    | Help => helpOpts
    | Info => infoOpts
    | Init => initOpts
    | Install => installOpts
    | List => listOpts
    | Uninstall => uninstallOpts
    | Update => updateOpts
    | Upgrade => upgradeOpts
    | Upload => uploadOpts;

val allCommandStrings = List.map commandString allCommands;

local
  val allCommandCommandStrings =
      List.map (fn c => (c, commandString c)) allCommands;
in
  fun commandFromString s =
      case List.find (equal s o snd) allCommandCommandStrings of
        SOME (c,_) => SOME c
      | NONE => NONE;
end;

val allCommandOpts =
    let
      fun mk cmd = annotateOptions (commandString cmd) (commandOpts cmd)
    in
      List.concat (List.map mk allCommands)
    end;

(* ------------------------------------------------------------------------- *)
(* Program options.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  open Useful Options;
in
  val globalOpts : opt list =
      [{switches = ["-d","--root-dir"], arguments = ["DIR"],
        description = "set package repo directory",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => rootDirectoryOption := SOME s)},
       {switches = ["--repo"], arguments = ["REPO"],
        description = "use given remote package repo",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => addRemote s)},
       {switches = ["--show-types"], arguments = [],
        description = "annotate every term variable with its type",
        processor =
          beginOpt endOpt
            (fn _ => Var.showTypes := true)}];
end;

local
  fun mkProgramOptions header footer opts =
      {name = program,
       version = versionString,
       header = "usage: " ^ program ^ " " ^ header ^ "\n",
       footer = footer,
       options = opts @ Options.basicOptions};

  val globalUsage = "[global options] command [command options] INPUT ...";

  val globalHeader =
      let
        fun f cmd =
            ["  " ^ program ^ " " ^ commandString cmd ^ " ...",
             " " ^ commandDescription cmd]

        val alignment =
            [{leftAlign = true, padChar = #"."},
             {leftAlign = true, padChar = #" "}]

        val table = alignTable alignment (List.map f allCommands)
      in
        globalUsage ^ "\n" ^
        "where the available commands are:\n" ^
        join "\n" table ^ "\n"
      end;

  val globalFooter = "";

  val allFormatsFooter =
      describeInputFormat ^ "\n" ^
      describeNameFormat ^ ".\n" ^
      describeVersionFormat ^ ".\n" ^
      describeFileFormat ^ ".\n" ^
      describeInfoFormat ^ ".\n" ^
      describeDirFormat ^ ".\n" ^
      describeRepoFormat ^ ".\n" ^
      describeQueryFormat;
in
  val globalOptions =
      mkProgramOptions
        (globalHeader ^ "Displaying global options:")
        globalFooter
        globalOpts;

  fun commandOptions cmd =
      let
        val header =
            commandString cmd ^ " [" ^ commandString cmd ^ " options]" ^
            commandArgs cmd ^ "\n" ^
            capitalize (commandDescription cmd) ^ ".\n" ^
            "Displaying " ^ commandString cmd ^ " options:"

        val footer = commandFooter cmd

        val opts = commandOpts cmd
      in
        mkProgramOptions header footer opts
      end;

  fun programOptions () =
      let
        val header = globalHeader ^ "Displaying global options:"

        val footer = globalFooter

        val opts = globalOpts
      in
        mkProgramOptions header footer opts
      end;

  fun allCommandOptions () =
      let
        val header = globalHeader ^ "Displaying all options:"

        val footer = globalFooter ^ allFormatsFooter;

        val opts = annotateOptions "global" globalOpts @ allCommandOpts
      in
        mkProgramOptions header footer opts
      end;
end;

fun exit x : unit = Options.exit (programOptions ()) x;

fun succeed () = Options.succeed (programOptions ());

fun fail mesg = Options.fail (programOptions ()) mesg;

fun usage mesg = Options.usage (programOptions ()) mesg;

fun commandUsage cmd mesg = Options.usage (commandOptions cmd) mesg;

fun allCommandHelp mesg =
    Options.exit (allCommandOptions ())
      {message = SOME mesg, usage = true, success = true};

(* ------------------------------------------------------------------------- *)
(* Cleaning up staged packages.                                              *)
(* ------------------------------------------------------------------------- *)

local
  fun cleanupInput inp =
      case inp of
        ArticleInput _ => raise Error "cannot clean up an article file"
      | PackageInput _ => raise Error "cannot clean up an installed package"
      | PackageNameInput _ => raise Error "cannot clean up a package name"
      | PackageQueryInput _ => raise Error "cannot clean up a package query"
      | StagedPackageInput namever => namever
      | TarballInput _ => raise Error "cannot clean up a tarball"
      | TheoryInput _ => raise Error "cannot clean up a theory source file";
in
  fun cleanup nameverl =
      let
        val repo = repository ()

        val nameverl =
            if not (List.null nameverl) then List.map cleanupInput nameverl
            else
              let
                val namevers = Repository.listStaged repo {maxAge = NONE}
              in
                PackageNameVersionSet.toList namevers
              end

        val () = List.app (cleanupStagedPackage repo) nameverl
      in
        ()
      end
      handle Error err =>
        raise Error (err ^ "\ncleaning up failed");
end;

(* ------------------------------------------------------------------------- *)
(* Exporting installed packages.                                             *)
(* ------------------------------------------------------------------------- *)

local
  fun exportInput inp =
      case inp of
        ArticleInput _ => raise Error "cannot export an article file"
      | PackageInput namever => namever
      | PackageNameInput name => getLatestVersion name
      | PackageQueryInput _ => raise Error "cannot export a package query"
      | StagedPackageInput _ => raise Error "cannot export a staged package"
      | TarballInput _ => raise Error "cannot export a tarball"
      | TheoryInput _ => raise Error "cannot export a theory source file";
in
  fun export inp =
      let
        val repo = repository ()

        val namever = exportInput inp

        val name = PackageNameVersion.name namever
      in
        if PackageName.isHaskell name then Haskell.export repo namever
        else raise Error ("unknown export type: " ^ PackageName.toString name)
      end
      handle Error err =>
        raise Error (err ^ "\npackage export failed");
end;

(* ------------------------------------------------------------------------- *)
(* Displaying command help.                                                  *)
(* ------------------------------------------------------------------------- *)

fun help () = allCommandHelp "displaying help on all available commands";

(* ------------------------------------------------------------------------- *)
(* Displaying package information.                                           *)
(* ------------------------------------------------------------------------- *)

local
  type 'a cache = 'a option option ref;

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
    val cacheSavable : bool cache = ref NONE;

    fun computeSavable () = NONE;
  in
    fun setSavable sav = cacheSavable := SOME (SOME sav);

    fun getSavable () =
        case getCached cacheSavable computeSavable () of
          SOME sav => sav
        | NONE => raise Bug "opentheory.info.getSavable";
  end;

  local
    val cachePackage : Package.package cache = ref NONE;

    fun computePackage () = NONE;
  in
    fun setPackage pkg = cachePackage := SOME (SOME pkg);

    val getPackage = getCached cachePackage computePackage;
  end;

  local
    val cacheTarball : PackageTarball.tarball cache = ref NONE;

    fun computeTarball () =
        case getPackage () of
          SOME pkg => SOME (Package.tarball pkg)
        | NONE => NONE;
  in
    fun setTarball tar = cacheTarball := SOME (SOME tar);

    val getTarball = getCached cacheTarball computeTarball;
  end;

  local
    val cacheInformation : PackageInformation.information cache = ref NONE;

    fun computeInformation () =
        case getPackage () of
          SOME pkg => SOME (Package.information pkg)
        | NONE => NONE;
  in
    fun setInformation info = cacheInformation := SOME (SOME info);

    val getInformation = getCached cacheInformation computeInformation;
  end;

  local
    val cacheNameVersion : PackageNameVersion.nameVersion cache =
        ref NONE;

    fun computeNameVersion () =
        case getPackage () of
          SOME pkg => SOME (Package.nameVersion pkg)
        | NONE =>
          case getInformation () of
            SOME info => total PackageInformation.nameVersion info
          | NONE =>
            case getTarball () of
              SOME tar => total PackageTarball.nameVersion tar
            | NONE => NONE;
  in
    fun setNameVersion namever = cacheNameVersion := SOME (SOME namever);

    val getNameVersion = getCached cacheNameVersion computeNameVersion;
  end;

  local
    val cacheChecksum : Checksum.checksum cache = ref NONE;

    fun computeChecksum () =
        case getPackage () of
          SOME pkg => SOME (Package.checksum pkg)
        | NONE =>
          case getTarball () of
            SOME tar => SOME (PackageTarball.checksum tar)
          | NONE => NONE;
  in
    val getChecksum = getCached cacheChecksum computeChecksum;
  end;

  local
    val cacheRequires : PackageName.name list cache = ref NONE;

    fun computeRequires () =
        case getInformation () of
          SOME info => SOME (PackageInformation.requires info)
        | NONE => NONE;
  in
    val getRequires = getCached cacheRequires computeRequires;
  end;

  local
    val cacheTags : PackageTag.tag list cache = ref NONE;

    fun computeTags () =
        case getInformation () of
          SOME info => SOME (PackageInformation.tags info)
        | NONE => NONE;
  in
    val getTags = getCached cacheTags computeTags;
  end;

  local
    val cacheDirectory : {directory : string} cache = ref NONE;

    fun computeDirectory () =
        case getPackage () of
          SOME pkg => SOME (Package.directory pkg)
        | NONE => NONE;
  in
    fun setDirectory dir = cacheDirectory := SOME (SOME dir);

    val getDirectory = getCached cacheDirectory computeDirectory;
  end;

  local
    val cacheTheoryFile : {filename : string} cache = ref NONE;

    fun computeTheoryFile () =
        case getPackage () of
          SOME pkg => SOME (Package.theoryFile pkg)
        | NONE => NONE;
  in
    fun setTheoryFile file = cacheTheoryFile := SOME (SOME file);

    val getTheoryFile = getCached cacheTheoryFile computeTheoryFile;
  end;

  local
    val cacheFiles : {filename : string} list cache = ref NONE;

    fun computeFiles () =
        case getPackage () of
          SOME pkg => SOME (Package.allFiles pkg)
        | NONE =>
          case getTarball () of
            SOME tar => SOME (PackageTarball.allFiles tar)
          | NONE =>
            case getTheoryFile () of
              NONE => NONE
            | SOME thy =>
              case getInformation () of
                NONE => NONE
              | SOME info =>
                let
                  val arts = PackageInformation.articleFiles info
                  and extras = PackageInformation.extraFiles info
                in
                  SOME (thy :: arts @ List.map PackageExtra.filename extras)
                end;
  in
    val getFiles = getCached cacheFiles computeFiles;
  end;

  local
    val cacheTheories : PackageTheory.theory list cache =
        ref NONE;

    fun upgradeTheories info =
        if not (!upgradeTheoryInfo) then info
        else
          let
            val repo = repository ()

            val info =
                case Repository.upgradeTheory repo info of
                  SOME i => i
                | NONE =>
                  let
                    val err = "no upgrade possible: theory source is up to date"
                  in
                    raise Error err
                  end
          in
            info
          end;

    fun optimizeTheories thys =
        if !preserveTheoryInfo then SOME thys
        else
          case getDirectory () of
            NONE => NONE
          | SOME {directory = dir} =>
            let
              val fndr = finder ()

              val graph =
                  PackageTheoryGraph.mk
                    {finder = fndr,
                     directory = dir,
                     theories = thys}

              val graph = PackageTheoryGraph.unwind graph
            in
              SOME (PackageTheoryGraph.theories graph)
            end;

    fun computeTheories () =
        case getPackage () of
          SOME pkg =>
          let
            val info = Package.information pkg

            val info = upgradeTheories info

            val thys = PackageInformation.theories info
          in
            SOME thys
          end
        | NONE =>
          case getInformation () of
            SOME info =>
            let
              val info = upgradeTheories info

              val thys = PackageInformation.theories info
            in
              optimizeTheories thys
            end
          | NONE => NONE;
  in
    val getTheories = getCached cacheTheories computeTheories;
  end;

  local
    val cacheTheory : (TheoryGraph.graph * Theory.theory) cache =
        ref NONE;

    fun computeTheory () =
        case getDirectory () of
          NONE => NONE
        | SOME {directory = dir} =>
          case getTheories () of
            NONE => NONE
          | SOME thys =>
              let
                val sav = getSavable ()

                val fndr = finder ()

                val graph = TheoryGraph.empty {savable = sav}

                val imps = TheorySet.empty

                val int = Interpretation.natural

                val (graph,env) =
                    TheoryGraph.importTheories fndr graph
                      {directory = dir,
                       imports = imps,
                       interpretation = int,
                       theories = thys}

                val thy = TheoryGraph.mainEnvironment env
              in
                SOME (graph,thy)
              end;
  in
    val getTheory = getCached cacheTheory computeTheory;
  end;

  local
    val cacheArticle : Article.article cache = ref NONE;

    fun computeArticle () =
        case getTheory () of
          SOME (_,thy) => SOME (Theory.article thy)
        | NONE => NONE;
  in
    fun setArticle art = cacheArticle := SOME (SOME art);

    val getArticle = getCached cacheArticle computeArticle;
  end;

  local
    val cacheThms : Thms.thms cache = ref NONE;

    fun computeThms () =
        case getArticle () of
          SOME art => SOME (Article.thms art)
        | NONE => NONE;
  in
    val getThms = getCached cacheThms computeThms;
  end;

  local
    val cacheTheorems : PackageTheorems.theorems cache = ref NONE;

    fun computeTheorems () =
        case getPackage () of
          SOME pkg => SOME (Package.theorems pkg)
        | NONE =>
          case getNameVersion () of
            NONE => NONE
          | SOME nv =>
            case getThms () of
              NONE => NONE
            | SOME ths =>
              let
                val seqs = Sequents.fromThms ths
              in
                SOME (PackageTheorems.mk nv seqs)
              end;
  in
    val getTheorems = getCached cacheTheorems computeTheorems;
  end;

  local
    val cacheSummary : Summary.summary cache = ref NONE;

    fun computeSummary () =
        case getThms () of
          SOME ths => SOME (Summary.fromThms ths)
        | NONE => NONE;
  in
    val getSummary = getCached cacheSummary computeSummary;
  end;

  local
    val cacheRequiresTheorems : PackageTheorems.theorems list cache =
        ref NONE;

    fun computeRequiresTheorems () =
        case getRequires () of
          NONE => NONE
        | SOME reqs =>
          let
            val repo = repository ()
          in
            Repository.requiresTheorems repo reqs
          end;
  in
    val getRequiresTheorems =
        getCached cacheRequiresTheorems computeRequiresTheorems;
  end;

  local
    val cacheInference : Inference.inference cache = ref NONE;

    fun computeInference () =
        case getTheory () of
          SOME (graph,_) =>
          SOME (TheorySet.inference (TheoryGraph.theories graph))
        | NONE =>
          case getArticle () of
            SOME art => SOME (Article.inference art)
          | NONE => NONE;
  in
    val getInference = getCached cacheInference computeInference;
  end;

  local
    val cacheBrand : Name.name cache = ref NONE;

    fun computeBrand () =
        case getNameVersion () of
          SOME nv => SOME (PackageNameVersion.toGlobal nv)
        | NONE => SOME (Name.mkGlobal "unknown")
  in
    val getBrand = getCached cacheBrand computeBrand;
  end;

  local
    val cacheObjectTheorems : ObjectTheorems.theorems cache = ref NONE;

    fun computeObjectTheorems () =
        case getTheorems () of
          SOME ths => SOME (PackageTheorems.theorems ths)
        | NONE =>
          case getBrand () of
            NONE => NONE
          | SOME brand =>
            case getThms () of
              NONE => NONE
            | SOME ths =>
              let
                val seqs = Sequents.fromThms ths
              in
                SOME (ObjectTheorems.mk brand seqs)
              end;
  in
    val getObjectTheorems =
        getCached cacheObjectTheorems computeObjectTheorems;
  end;

  local
    val cacheObjectAssumptions : ObjectTheorems.theorems cache = ref NONE;

    fun computeObjectAssumptions () =
        case getBrand () of
          NONE => NONE
        | SOME brand =>
          case getSummary () of
            NONE => NONE
          | SOME sum =>
            let
              val seqs = Summary.requires sum
            in
              SOME (ObjectTheorems.mk brand seqs)
            end;
  in
    val getObjectAssumptions =
        getCached cacheObjectAssumptions computeObjectAssumptions;
  end;

  fun processFormat (InfoFormat items) =
      let
        fun mkItem item =
            case item of
              ChecksumItem =>
              let
                val chk =
                    case getChecksum () of
                      SOME c => c
                    | NONE => raise Error "no checksum information available"
              in
                Checksum.toString chk
              end
            | DescriptionItem =>
              let
                val info =
                    case getInformation () of
                      SOME i => i
                    | NONE => raise Error "no package information available"

                val {description} = PackageInformation.description info
              in
                description
              end
            | EmptyItem =>
              let
                val info =
                    case getInformation () of
                      SOME i => i
                    | NONE => raise Error "no package information available"

                val empty = PackageInformation.emptyTheories info
              in
                emptyToString empty
              end
            | NameItem =>
              let
                val namever =
                    case getNameVersion () of
                      SOME nv => nv
                    | NONE => raise Error "no name information available"

                val name = PackageNameVersion.name namever
              in
                PackageName.toString name
              end
            | SeparatorItem s => s
            | VersionItem =>
              let
                val namever =
                    case getNameVersion () of
                      SOME nv => nv
                    | NONE => raise Error "no name information available"

                val version = PackageNameVersion.version namever
              in
                PackageVersion.toString version
              end
      in
        List.map mkItem items
      end;

  fun outputPackageNameVersionSet namevers file =
      let
        fun mk nv = PackageNameVersion.toString nv ^ "\n"

        val namevers = PackageNameVersionSet.toList namevers

        val strm = Stream.map mk (Stream.fromList namevers)
      in
        Stream.toTextFile file strm
      end;

  fun processInfoOutput (inf,file) =
      case inf of
        ArticleInfo vo =>
        let
          val art =
              case getArticle () of
                SOME a => a
              | NONE => raise Error "no article information available"

          val version = Option.getOpt (vo,ArticleVersion.writeDefault)

          val {filename} = file
        in
          Article.toTextFile
            {article = art,
             version = version,
             filename = filename}
        end
      | AssumptionsInfo =>
        let
          val ths =
              case getObjectAssumptions () of
                SOME ths => ths
              | NONE => raise Error "no assumption information available"

          val {filename} = file
        in
          ObjectTheorems.toTextFile {theorems = ths, filename = filename}
        end
      | DocumentInfo =>
        let
          val info = getInformation ()

          val chk = getChecksum ()

          val sum =
              case getTheory () of
                SOME (_,t) => TheoryGraph.summary t
              | NONE =>
                case getSummary () of
                  SOME s =>
                  let
                    val e : PackageSummary.sequentSource = SequentMap.new ()

                    val ps =
                        PackageSummary.Summary'
                          {summary = s,
                           requires = e,
                           provides = e}
                  in
                    PackageSummary.mk ps
                  end
                | NONE => raise Error "no theory information available"

          val files =
              let
                val theory =
                    case getDirectory () of
                      NONE => NONE
                    | SOME {directory = dir} =>
                      case getTheoryFile () of
                        NONE => NONE
                      | SOME {filename = file} =>
                        SOME (OS.Path.joinDirFile {dir = dir, file = file})

                val tarball =
                    case getTarball () of
                      NONE => NONE
                    | SOME tar =>
                      let
                        val {filename} = PackageTarball.filename tar
                      in
                        SOME filename
                      end
              in
                {theory = theory,
                 tarball = theory}
              end

          val tool = versionHtml

          val doc =
              PackageDocument.Document'
                {information = info,
                 checksum = chk,
                 summary = sum,
                 files = files,
                 tool = tool}

          val doc = PackageDocument.mk doc

          val {filename} = file
        in
          PackageDocument.toHtmlFile
            {document = doc,
             filename = filename}
        end
      | FilesInfo =>
        let
          fun mk {filename} = filename ^ "\n"

          val files =
              case getFiles () of
                SOME f => f
              | NONE => raise Error "no files information available"

          val strm = Stream.map mk (Stream.fromList files)
        in
          Stream.toTextFile file strm
        end
      | FormatInfo fmt =>
        let
          val sl = processFormat fmt @ ["\n"]

          val strm = Stream.fromList sl
        in
          Stream.toTextFile file strm
        end
      | IncludesInfo =>
        let
          fun mk (nv,_) = PackageNameVersion.toString nv ^ "\n"

          val info =
              case getInformation () of
                SOME i => i
              | NONE => raise Error "no includes information available"

          val incs = PackageInformation.includes info

          val strm = Stream.map mk (Stream.fromList incs)
        in
          Stream.toTextFile file strm
        end
      | InferenceInfo =>
        let
          val inf =
              case getInference () of
                SOME i => i
              | NONE => raise Error "no inference information available"

          val strm = Print.toStream Inference.pp inf
        in
          Stream.toTextFile file strm
        end
      | RequiresInfo =>
        let
          fun checkPrevious oldest ths vs =
              if Queue.null ths then oldest
              else
                let
                  val (th,ths) = Queue.hdTl ths

                  val nv = PackageTheorems.package th
                in
                  case previousVersion nv of
                    NONE =>
                    let
                      val n = PackageNameVersion.name nv
                      and v = PackageNameVersion.version nv

                      val oldest = PackageNameMap.insert oldest (n,v)
                    in
                      checkPrevious oldest ths vs
                    end
                  | SOME nv' =>
                    let
                      val repo = repository ()

                      val pkg = Repository.get repo nv'

                      val th = Package.theorems pkg
                    in
                      case total (PackageTheorems.addVersion vs) th of
                        NONE =>
                        let
                          val n = PackageNameVersion.name nv
                          and v = PackageNameVersion.version nv

                          val oldest = PackageNameMap.insert oldest (n,v)
                        in
                          checkPrevious oldest ths vs
                        end
                      | SOME vs =>
                        let
                          val ths = Queue.add th ths
                        in
                          checkPrevious oldest ths vs
                        end
                    end
                end

          val ths =
              case getRequiresTheorems () of
                SOME r => r
              | NONE => raise Error "no requires information available"

          val sum =
              case getSummary () of
                SOME s => s
              | NONE => raise Error "no theory information available"

          val vs = PackageTheorems.mkVersions sum ths

          val oldest =
              checkPrevious (PackageNameMap.new ()) (Queue.fromList ths) vs

          fun mk th =
              let
                val nv = PackageTheorems.package th

                val n = PackageNameVersion.name nv
                and new = PackageNameVersion.version nv

                val old =
                    case PackageNameMap.peek oldest n of
                      SOME v => v
                    | NONE => raise Bug "opentheory.info.RequiresInfo.mk"
              in
                PackageName.toString n ^
                (if PackageVersion.equal new old then
                   " == " ^ PackageVersion.toString new
                 else
                   " >= " ^ PackageVersion.toString old ^
                   " /\\ <= " ^ PackageVersion.toString new) ^ "\n"
              end

          val strm = Stream.map mk (Stream.fromList ths)
        in
          Stream.toTextFile file strm
        end
      | SummaryInfo =>
        let
          val sum =
              case getSummary () of
                SOME s => s
              | NONE => raise Error "no theory information available"

          val grammar = infoSummaryGrammar ()

          val context =
              if !showAssumptionsInfo then Summary.NoContext
              else
                case getRequiresTheorems () of
                  NONE => Summary.NoContext
                | SOME ths => PackageTheorems.context sum ths

          val show =
              case getInformation () of
                SOME info => PackageInformation.show info
              | NONE => Show.default

          val {filename} = file
        in
          Summary.toTextFileWithGrammar grammar
            {context = context,
             show = show,
             summary = sum,
             filename = filename}
        end
      | SymbolsInfo =>
        let
          val art =
              case getArticle () of
                SOME a => a
              | NONE => raise Error "no article information available"

          val sym = Article.symbols art

          val () = ObjectExport.warnClashingSymbols sym

          val strm = Print.toStream ObjectExport.ppSymbols sym
        in
          Stream.toTextFile file strm
        end
      | TagsInfo =>
        let
          val tags =
              case getTags () of
                SOME t => t
              | NONE => raise Error "no package information available"

          val strm = Print.toStream PackageTag.ppList tags
        in
          Stream.toTextFile file strm
        end
      | TheoremsInfo =>
        let
          val ths =
              case getObjectTheorems () of
                SOME ths => ths
              | NONE => raise Error "no theorem information available"

          val {filename} = file
        in
          ObjectTheorems.toTextFile {theorems = ths, filename = filename}
        end
      | TheoryInfo =>
        let
          val thys =
              case getTheories () of
                SOME t => t
              | NONE => raise Error "no theory source information available"

          val strm = Print.toStream PackageTheory.ppList thys
        in
          Stream.toTextFile file strm
        end;

  fun processInfoOutputList infs =
      let
        val () = List.app processInfoOutput infs
      in
        ()
      end;

  fun infoArticle {filename} infs =
      let
        val sav = getSavable ()
        and imp = Article.empty
        and int = Interpretation.natural

        val art =
            Article.fromTextFile
              {savable = sav,
               import = imp,
               interpretation = int,
               filename = filename}

        val () = setArticle art
      in
        processInfoOutputList infs
      end;

  fun infoPackage namever infs =
      let
        val fndr = finder ()
      in
        case PackageFinder.find fndr namever NONE of
          NONE =>
          let
            val err =
                "package " ^ PackageNameVersion.toString namever ^
                " is not installed"
          in
            raise Error err
          end
        | SOME pkg =>
          let
            val () = setPackage pkg
          in
            processInfoOutputList infs
          end
      end;

  fun infoPackageName name infs =
      let
        val namever = getLatestVersion name
      in
        infoPackage namever infs
      end;

  fun infoStagedPackage namever infs =
      let
        val fndr = stagedFinder ()
      in
        case PackageFinder.find fndr namever NONE of
          NONE =>
          let
            val err =
                "package " ^ PackageNameVersion.toString namever ^
                " is not staged for installation"
          in
            raise Error err
          end
        | SOME pkg =>
          let
            val () = setPackage pkg
          in
            processInfoOutputList infs
          end
      end;

  fun infoTarball {filename = tarFile} infs =
      let
        val sys = system ()

        val tar =
            PackageTarball.mk
              {system = sys,
               filename = tarFile,
               checksum = NONE}

        val () = setTarball tar
      in
        processInfoOutputList infs
      end;

  fun infoTheory {filename = thyFile} infs =
      let
        val info = PackageInformation.fromTextFile {filename = thyFile}
        and dir = OS.Path.dir thyFile
        and file = OS.Path.file thyFile

        val () = setInformation info
        and () = setDirectory {directory = dir}
        and () = setTheoryFile {filename = file}
      in
        processInfoOutputList infs
      end;
in
  fun info inp =
      let
        val infs = readInfoOutputList inp

        val sav = List.exists (savableInfo o fst) infs

        val () = setSavable sav
      in
        case inp of
          ArticleInput file => infoArticle file infs
        | PackageInput namever => infoPackage namever infs
        | PackageNameInput name => infoPackageName name infs
        | PackageQueryInput _ =>
          raise Error "cannot display information about a package query"
        | StagedPackageInput namever => infoStagedPackage namever infs
        | TarballInput file => infoTarball file infs
        | TheoryInput file => infoTheory file infs
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Initializing a package directory.                                         *)
(* ------------------------------------------------------------------------- *)

fun init () =
    let
      val {directory = r, autoInit = _} = rootDirectory ()

      val x = initRepository {rootDirectory = r}

      val msg =
          "initialized new package repo " ^
          Print.toString Repository.pp x ^
          (if !remoteInit then " for remote use" else "")

      val () = chat msg
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Uninstalling packages.                                                    *)
(* ------------------------------------------------------------------------- *)

local
  fun uninstallInput inp =
      case inp of
        ArticleInput _ => raise Error "cannot uninstall an article file"
      | PackageInput namever =>
        RepositoryQuery.Constant (RepositoryQuery.NameVersion namever)
      | PackageNameInput name =>
        RepositoryQuery.Constant (RepositoryQuery.Name name)
      | PackageQueryInput query => query
      | StagedPackageInput _ =>
        let
          val err = "cannot uninstall a staged package (use cleanup instead)"
        in
          raise Error err
        end
      | TarballInput _ => raise Error "cannot uninstall a tarball"
      | TheoryInput _ => raise Error "cannot uninstall a theory source file";

  fun complain errs =
      if RepositoryError.isClean errs then ()
      else
        let
          val s = RepositoryError.report errs
        in
          if RepositoryError.fatal errs then raise Error s
          else chat ("package uninstall warnings:\n" ^ s)
        end;

  fun uninstallPackages repo namevers =
      let
        fun uninstall1 namever =
            let
              val auto = not (PackageNameVersionSet.member namever namevers)

              val () = Repository.uninstall repo namever

              val () =
                  let
                    val msg =
                        (if auto then "auto-" else "") ^
                        "uninstalled package " ^ PackageNameVersion.toString namever
                  in
                    chat msg
                  end
            in
              ()
            end

        val errs = Repository.checkUninstall repo namevers

        val errs =
            if not (!autoUninstall) then errs
            else
              let
                val (_,errs) = RepositoryError.removeInstalledUser errs
              in
                errs
              end

        val () = complain errs

        val namevers =
            if not (!autoUninstall) then namevers
            else Repository.includedByRTC repo namevers

        val namevers = List.rev (Repository.includeOrder repo namevers)

        val () = List.app uninstall1 namevers
      in
        ()
      end;
in
  fun uninstallPackage namever =
      let
        val repo = repository ()
      in
        uninstallPackages repo (PackageNameVersionSet.singleton namever)
      end;

  fun uninstall inp =
      let
        val repo = repository ()

        val query = uninstallInput inp

        val namevers = evaluateQuery query
      in
        if PackageNameVersionSet.null namevers then
          raise Error "no matching installed packages"
        else
          uninstallPackages repo namevers
      end
      handle Error err =>
        raise Error (err ^ "\npackage uninstall failed");
end;

(* ------------------------------------------------------------------------- *)
(* Installing packages.                                                      *)
(* ------------------------------------------------------------------------- *)

local
  fun installAuto fndr namever chko =
      let
        val repo = repository ()

        val (rem,chk) = firstRemote namever chko

        val errs = Repository.checkStagePackage repo rem namever chk

        val () =
            if RepositoryError.isClean errs then ()
            else
              let
                val s = RepositoryError.report errs
              in
                if RepositoryError.fatal errs then raise Error s
                else chat ("included package " ^
                           PackageNameVersion.toString namever ^
                           " install warnings:\n" ^ s)
              end

        val tool = {tool = versionHtml}

        val () = Repository.stagePackage repo fndr rem namever chk tool

        val () = Repository.installStaged repo namever chk

        val () = chat ("auto-installed package " ^
                       PackageNameVersion.toString namever)
      in
        ()
      end;

  fun installAutoFinder fndr =
      let
        fun findOrInstall namever chko =
            case PackageFinder.find fndr namever chko of
              SOME pkg => SOME pkg
            | NONE =>
              let
                val inst = installAutoFinder fndr

                val () = installAuto inst namever chko

                val repo = repository ()
              in
                Repository.peek repo namever
              end
      in
        PackageFinder.mk findOrInstall
      end;
in
  fun installFinder () =
      let
        val fndr =
            if not (!stageInstall) then finder ()
            else possiblyStagedFinder ()
      in
        if not (!autoInstall) then fndr else installAutoFinder fndr
      end;
end;

fun installPackage rem namever chk =
    let
      val repo = repository ()

      val errs = Repository.checkStagePackage repo rem namever chk

      val errs =
          if not (!reinstall) then errs
          else
            let
              val (staged,errs) = RepositoryError.removeAlreadyStaged errs

              val () =
                  if not staged then ()
                  else Repository.cleanupStaged repo namever
            in
              errs
            end

      val (replace,errs) =
          if not (!reinstall) then (false,errs)
          else RepositoryError.removeAlreadyInstalled errs

      val () =
          if RepositoryError.isClean errs then ()
          else
            let
              val s = RepositoryError.report errs
            in
              if RepositoryError.fatal errs then raise Error s
              else chat ("package install warnings:\n" ^ s)
            end

      val () =
          if not replace then ()
          else uninstallPackage namever

      val fndr = installFinder ()

      val tool = {tool = versionHtml}

      val () = Repository.stagePackage repo fndr rem namever chk tool

      val () = Repository.installStaged repo namever chk

      val () =
          chat ((if replace then "re" else "") ^ "installed package " ^
                PackageNameVersion.toString namever)
    in
      ()
    end;

local
  fun installStagedPackage namever =
      let
        val () =
            if not (Option.isSome (!nameInstall)) then ()
            else raise Error "can't specify name for a staged package install"

        val () =
            if not (!stageInstall) then ()
            else raise Error "can't stage a staged package install"

        val () =
            if not (!reinstall) then ()
            else raise Error "can't reinstall a staged package install"

        val repo = repository ()

        val pkg =
            let
              val fndr = stagedFinder ()
              and chko = !checksumInstall
            in
              case PackageFinder.find fndr namever chko of
                SOME p => p
              | NONE =>
                let
                  val err =
                      "can't find staged package " ^
                      PackageNameVersion.toString namever
                in
                  raise Error err
                end
            end

        val errs = Repository.checkInstallStaged repo namever

        val () =
            if RepositoryError.isClean errs then ()
            else
              let
                val s = RepositoryError.report errs
              in
                if RepositoryError.fatal errs then raise Error s
                else chat ("staged package install warnings:\n" ^ s)
              end

        val chk = Package.checksum pkg

        val () = Repository.installStaged repo namever chk

        val () =
            chat ("installed staged package " ^
                  PackageNameVersion.toString namever)
      in
        ()
      end
      handle Error err =>
        raise Error (err ^ "\nstaged package install failed");

  fun installPackageNameVersion namever =
      let
        val () =
            if not (Option.isSome (!nameInstall)) then ()
            else raise Error "can't specify name for a package install"

        val () =
            if not (!stageInstall) then ()
            else raise Error "can't stage a package install"

        val (rem,chk) = firstRemote namever (!checksumInstall)

        val () = installPackage rem namever chk
      in
        ()
      end
      handle Error err =>
        raise Error (err ^ "\npackage install failed");

  fun installPackageName name =
      let
        val () =
            if not (Option.isSome (!nameInstall)) then ()
            else raise Error "can't specify name for a package name install"

        val () =
            if not (!stageInstall) then ()
            else raise Error "can't stage a package name install"

        val () =
            case latestVersion name of
              SOME nv =>
              let
                val err =
                    "package " ^ PackageNameVersion.toString nv ^
                    " is already installed"
              in
                raise Error err
              end
            | NONE => ()
      in
        case latestVersionRemotes name (!checksumInstall) of
          NONE =>
          let
            val err =
                "can't find a version of package " ^
                PackageName.toString name

            val err =
                if not (Option.isSome (!checksumInstall)) then err
                else err ^ " with specified checksum"

            val err = err ^ " in any repo"
          in
            raise Error err
          end
        | SOME (rem,nv,chk) => installPackage rem nv chk
      end
      handle Error err =>
        raise Error (err ^ "\npackage name install failed");

  fun installTarball {filename = tarFile} =
      let
        val repo = repository ()
        and sys = system ()

        val tar =
            PackageTarball.mk
              {system = sys,
               filename = tarFile,
               checksum = NONE}

        val chk = PackageTarball.checksum tar

        val () =
            case !checksumInstall of
              NONE => ()
            | SOME chk' =>
              if Checksum.equal chk' chk then ()
              else raise Error "tarball checksum does not match"

        val namever = PackageTarball.nameVersion tar

        val () =
            case !nameInstall of
              NONE => ()
            | SOME namever' =>
              if PackageNameVersion.equal namever' namever then ()
              else raise Error "tarball name does not match"

        val errs = Repository.checkStageTarball repo tar

        val errs =
            if not (!reinstall) then errs
            else
              let
                val (staged,errs) = RepositoryError.removeAlreadyStaged errs

                val () =
                    if not staged then ()
                    else Repository.cleanupStaged repo namever
              in
                errs
              end

        val (replace,errs) =
            if not (!reinstall) then (false,errs)
            else RepositoryError.removeAlreadyInstalled errs

        val () =
            if RepositoryError.isClean errs then ()
            else
              let
                val s = RepositoryError.report errs
              in
                if RepositoryError.fatal errs then raise Error s
                else chat ("package install warnings:\n" ^ s)
              end

        val () =
            if not replace then ()
            else uninstallPackage namever

        val fndr = installFinder ()

        val tool = {tool = versionHtml}

        val () = Repository.stageTarball repo fndr tar tool

        val () =
            if !stageInstall then ()
            else Repository.installStaged repo namever chk

        val () =
            let
              val verb =
                  if !stageInstall then
                    (if replace then "uninstalled and staged" else "staged")
                  else
                    (if replace then "reinstalled" else "installed")

              val msg =
                  verb ^ " package " ^ PackageNameVersion.toString namever ^
                  " from tarball"
            in
              chat msg
            end
      in
        ()
      end
      handle Error err =>
        raise Error (err ^ "\npackage install from tarball failed");

  fun installTheory thyFile =
      let
        val () =
            if not (Option.isSome (!checksumInstall)) then ()
            else
              let
                val err =
                    "can't specify checksum for a theory source file install"
              in
                raise Error err
              end

        val () =
            if not (!stageInstall) then ()
            else
              let
                val err = "can't stage a theory source file install"
              in
                raise Error err
              end

        val repo = repository ()

        val info = PackageInformation.fromTextFile thyFile

        val errs = Repository.checkStageTheory repo (!nameInstall) info

        val (cleanup,errs) =
            if not (!reinstall) then (false,errs)
            else RepositoryError.removeAlreadyStaged errs

        val (replace,errs) =
            if not (!reinstall) then (false,errs)
            else RepositoryError.removeAlreadyInstalled errs

        val errs =
            if not (!autoInstall) then errs
            else snd (RepositoryError.removeUninstalledInclude errs)

        val () =
            if RepositoryError.isClean errs then ()
            else
              let
                val s = RepositoryError.report errs
              in
                if RepositoryError.fatal errs then raise Error s
                else chat ("package install warnings:\n" ^ s)
              end

        val namever = PackageInformation.nameVersion info

        val () = if cleanup then Repository.cleanupStaged repo namever else ()

        val () = if replace then uninstallPackage namever else ()

        val fndr = installFinder ()

        val srcDir =
            let
              val {filename} = thyFile
            in
              {directory = OS.Path.dir filename}
            end

        val tool = {tool = versionHtml}

        val chk = Repository.stageTheory repo fndr namever info srcDir tool

        val () = Repository.installStaged repo namever chk

        val () =
            let
              val msg =
                  (if replace then "re" else "") ^ "installed package " ^
                  PackageNameVersion.toString namever ^
                  " from theory source file"
            in
              chat msg
            end
      in
        ()
      end
      handle Error err =>
        raise Error (err ^ "\npackage install from theory source file failed");
in
  fun install inp =
      case inp of
        ArticleInput _ => raise Error "cannot install an article file"
      | PackageInput namever => installPackageNameVersion namever
      | PackageNameInput name => installPackageName name
      | PackageQueryInput _ => raise Error "cannot install a package query"
      | StagedPackageInput namever => installStagedPackage namever
      | TarballInput file => installTarball file
      | TheoryInput file => installTheory file;
end;

(* ------------------------------------------------------------------------- *)
(* Listing installed packages.                                               *)
(* ------------------------------------------------------------------------- *)

fun sortList repo pkgs ord =
    case ord of
      AlphabeticalList => PackageNameVersionSet.toList pkgs
    | IncludeList => Repository.includeOrder repo pkgs
    | DependencyList => Repository.dependencyOrder repo pkgs
    | ReverseList ord => List.rev (sortList repo pkgs ord);

local
  fun listInput inpo =
      case inpo of
        NONE => RepositoryQuery.Identity
      | SOME inp =>
        case inp of
          ArticleInput _ => raise Error "cannot list an article file"
        | PackageInput namever =>
          RepositoryQuery.Constant (RepositoryQuery.NameVersion namever)
        | PackageNameInput name =>
          RepositoryQuery.Constant (RepositoryQuery.Name name)
        | PackageQueryInput query => query
        | StagedPackageInput _ =>
          raise Error "cannot list a staged package"
        | TarballInput _ => raise Error "cannot list a tarball"
        | TheoryInput _ => raise Error "cannot list a theory source file";
in
  fun list inp =
      let
        val query = listInput inp

        val namevers = evaluateQuery query

        val repo = repository ()

        val namevers = sortList repo namevers (orderList ());

        val fmt = getFormatList ()

        val strm =
            let
              fun mk namever = packageToStringInfoFormat repo fmt namever ^ "\n"
            in
              Stream.map mk (Stream.fromList namevers)
            end

        val ref f = outputList
      in
        Stream.toTextFile {filename = f} strm
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Update remote repository package lists.                                   *)
(* ------------------------------------------------------------------------- *)

fun updateRemote rem =
    let
      val () = RepositoryRemote.update rem

      val () =
          let
            val msg =
                "updated package list for " ^ RepositoryRemote.toString rem
          in
            chat msg
          end
    in
      ()
    end
    handle Error err =>
      raise Error (err ^ "\nrepo update failed");

fun update () =
    let
      val rems = remotes ()
    in
      List.app updateRemote rems
    end;

(* ------------------------------------------------------------------------- *)
(* Upgrade installed packages with later versions on a remote repository.    *)
(* ------------------------------------------------------------------------- *)

local
  fun upgradeInput inpo =
      case inpo of
        NONE => RepositoryQuery.Upgradable
      | SOME inp =>
        case inp of
          ArticleInput _ => raise Error "cannot upgrade an article file"
        | PackageInput namever =>
          RepositoryQuery.Constant (RepositoryQuery.NameVersion namever)
        | PackageNameInput name =>
          RepositoryQuery.Constant (RepositoryQuery.Name name)
        | PackageQueryInput query => query
        | StagedPackageInput _ => raise Error "cannot upgrade a staged package"
        | TarballInput _ => raise Error "cannot upgrade a tarball"
        | TheoryInput _ => raise Error "cannot upgrade a theory source file";

  fun upgradeList namevers =
      let
        fun upgradeName name =
            case latestVersionRemotes name NONE of
              NONE => ()
            | SOME (rem,nvr,chk) =>
              let
                val nvl =
                    case latestVersion name of
                      SOME nv => nv
                    | NONE => raise Bug "opentheory.upgrade: not found"

                val vl = PackageNameVersion.version nvl
                and vr = PackageNameVersion.version nvr
              in
                case PackageVersion.compare (vl,vr) of
                  LESS => installPackage rem nvr chk
                | _ => ()
              end

        fun upgrade1 (namever,names) =
            let
              val name = PackageNameVersion.name namever
            in
              if PackageNameSet.member name names then names
              else
                let
                  val () = upgradeName name
                in
                  PackageNameSet.add names name
                end
            end

        val _ = List.foldl upgrade1 PackageNameSet.empty namevers
      in
        ()
      end;
in
  fun upgrade inp =
      let
        val repo = repository ()

        val query = upgradeInput inp

        val namevers = evaluateQuery query
      in
        case Repository.includeOrder repo namevers of
          [] => raise Error "no matching installed packages"
        | namevers => upgradeList namevers
      end
      handle Error err =>
        raise Error (err ^ "\npackage upgrade failed");
end;

(* ------------------------------------------------------------------------- *)
(* Upload installed packages to a remote repository.                         *)
(* ------------------------------------------------------------------------- *)

local
  fun uploadInput inp =
      case inp of
        ArticleInput _ => raise Error "cannot upload an article file"
      | PackageInput namever => namever
      | PackageNameInput name => getLatestVersion name
      | PackageQueryInput _ => raise Error "cannot upload a package query"
      | StagedPackageInput _ => raise Error "cannot upload a staged package"
      | TarballInput _ => raise Error "cannot upload a tarball"
      | TheoryInput _ => raise Error "cannot upload a theory source file";

  fun computeSupport repo rem namevers =
      let
        fun notInRepo nv = not (Repository.member nv repo)

        fun notInRemote nv = not (RepositoryRemote.member nv rem)

        val (unknown,namevers) = List.partition notInRepo namevers

        val namevers = PackageNameVersionSet.fromList namevers

        val namevers =
            let
              val subs =
                  case !setUpload of
                    ManualUpload => PackageNameVersionSet.empty
                  | SubtheoryUpload => Repository.subtheoriesRTC repo namevers

              val subs = PackageNameVersionSet.filter notInRemote subs
            in
              PackageNameVersionSet.union subs namevers
            end

        val support =
            let
              val incs = Repository.includesRTC repo namevers

              val incs = PackageNameVersionSet.filter notInRemote incs

              val incs = PackageNameVersionSet.difference incs namevers
            in
              Repository.dependencyOrder repo incs
            end

        val namevers = unknown @ Repository.dependencyOrder repo namevers
      in
        (support,namevers)
      end;

  fun summarizeUpload upl =
      let
        val msg = Print.toString RepositoryUpload.pp upl

        val () = chat msg

        val () = TextIO.flushOut TextIO.stdOut
      in
        ()
      end;

  fun askToConfirmUpload () =
      let
        val () = TextIO.output (TextIO.stdOut, "Continue [y/N]? ")

        val () = TextIO.flushOut TextIO.stdOut

        val s =
            case TextIO.inputLine TextIO.stdIn of
              SOME s => String.map Char.toLower s
            | NONE => raise Error "standard input terminated"
      in
        if s = "y\n" then true
        else if s = "n\n" orelse s = "\n" then false
        else askToConfirmUpload ()
      end;
in
  fun upload inps =
      let
        val repo = repository ()

        val rem = remote ()

        val namevers = List.map uploadInput inps

        val () = RepositoryRemote.update rem

        val (support,namevers) = computeSupport repo rem namevers

        val upl =
            RepositoryUpload.mk
              {repository = repo,
               remote = rem,
               support = support,
               packages = namevers}

        val errs = RepositoryUpload.check upl

        val () =
            if RepositoryError.isClean errs then ()
            else
              let
                val s = RepositoryError.report errs
              in
                if RepositoryError.fatal errs then raise Error s
                else chat ("package upload warnings:\n" ^ s)
              end

        val () = summarizeUpload upl

        val proceed = not (!confirmUpload) orelse askToConfirmUpload ()
      in
        if not proceed then ()
        else
          let
            val () = RepositoryUpload.upload upl

            val () = RepositoryRemote.update rem
          in
            ()
          end
      end
      handle Error err =>
        raise Error (err ^ "\npackage upload failed");
end;

(* ------------------------------------------------------------------------- *)
(* Top level.                                                                *)
(* ------------------------------------------------------------------------- *)

val () =
let
  val work = CommandLine.arguments ();

  (* Process global options *)

  val (_,work) = Options.processOptions globalOptions work

  (* Read the command *)

  val (cmd,work) =
      case work of
        [] => usage "no command specified"
      | s :: work =>
        case commandFromString s of
          SOME cmd => (cmd,work)
        | NONE => usage ("bad command specified: \"" ^ s ^ "\"")

  (* Process command options *)

  val (_,work) = Options.processOptions (commandOptions cmd) work

  val work =
      List.map fromStringInput work
      handle Error err => commandUsage cmd err

  (* Process command options *)

  val () =
      case (cmd,work) of
        (Cleanup,pkgs) => cleanup pkgs
      | (Export,[pkg]) => export pkg
      | (Help,[]) => help ()
      | (Info,[inp]) => info inp
      | (Init,[]) => init ()
      | (Install,[inp]) => install inp
      | (List,[]) => list NONE
      | (List,[inp]) => list (SOME inp)
      | (Uninstall,[inp]) => uninstall inp
      | (Update,[]) => update ()
      | (Upgrade,[]) => upgrade NONE
      | (Upgrade,[inp]) => upgrade (SOME inp)
      | (Upload, pkgs as _ :: _) => upload pkgs
      | _ =>
        let
          val err = "bad arguments for " ^ commandString cmd ^ " command"
        in
          commandUsage cmd err
        end
in
  succeed ()
end
handle Error s => die (program^" failed:\n" ^ s)
     | Bug s => die ("BUG found in "^program^" program:\n" ^ s)
     | e => die (program^" exception:\n"^ exnMessage e);
