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

val version = "1.2";

val release = " (release 20121011)";

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
    "DIR is any directory on the file system";

val describeFileFormat =
    "FILE is any filename; use - to read from stdin or write to stdout";

val describeRepoFormat =
    "REPO is the name of any repo in the config file (e.g., gilith)";

val describeNameFormat =
    "NAME is any package name (e.g., base)";

val describeVersionFormat =
    "VERSION is any package version (e.g., 1.0)";

val describeQueryFormat =
    "QUERY represents a subset S of the installed theory packages P, as follows:\n" ^
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
    "  || WellFounded  // are all assumptions satisfied and inputs grounded?\n" ^
    "  || OnRepo       // is there a theory package with the same name on the repo?\n" ^
    "  || IdenticalOnRepo\n" ^
    "                  // is this exact same theory package on the repo?\n" ^
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
  | PackageQueryInput of DirectoryQuery.function
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
                case total DirectoryQuery.fromString inp of
                  SOME query => PackageQueryInput query
                | NONE =>
                  let
                    val f = {filename = inp}
                  in
                    if Article.isFilename f then ArticleInput f
                    else if PackageTarball.isFilename f then TarballInput f
                    else if Package.isFilename f then TheoryInput f
                    else raise Error ("unknown type of input: " ^ inp)
                  end;

val describeInputFormat =
    "INPUT is one of the following forms:\n" ^
    "  1. A theory package: NAME-VERSION or NAME (for the latest version)\n" ^
    "  2. A theory file: FILE.thy or theory:FILE\n" ^
    "  3. A proof article file: FILE.art or article:FILE\n" ^
    "  4. A theory package tarball: FILE.tgz or tarball:FILE\n" ^
    "  5. A theory package staged for installation: staged:NAME-VERSION";

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
    "FORMAT is any string containing " ^
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

(* ------------------------------------------------------------------------- *)
(* Clean up a staged package.                                                *)
(* ------------------------------------------------------------------------- *)

fun cleanupStagedPackage dir nv =
    let
      val () = Directory.cleanupStaged dir nv

      val mesg =
          "cleaned up staged package " ^ PackageNameVersion.toString nv

      val () = chat mesg
    in
      ()
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
(* Initializing a package directory.                                         *)
(* ------------------------------------------------------------------------- *)

val repoInit = ref DirectoryConfig.default;

fun initDirectory {rootDirectory = r} =
    let
      val c = !repoInit

      val () = Directory.create {rootDirectory = r, config = c}
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Package directory.                                                        *)
(* ------------------------------------------------------------------------- *)

val directory =
    let
      fun existsDirectory d =
          OS.FileSys.isDir d
          handle OS.SysErr _ => false

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
                   if existsDirectory r then
                     let
                       val dir = Directory.mk {rootDirectory = r}

                       val () =
                           let
                             val cfg = Directory.config dir

                             val cfg = DirectoryConfig.cleanup cfg
                           in
                             case DirectoryConfig.autoCleanup cfg of
                               NONE => ()
                             | SOME t =>
                               let
                                 val maxAge = {maxAge = SOME t}

                                 val nvs = Directory.listStaged dir maxAge

                                 val () =
                                     PackageNameVersionSet.app
                                       (cleanupStagedPackage dir) nvs
                               in
                                 ()
                               end
                           end
                     in
                       dir
                     end
                   else if autoInit then
                     let
                       val () = initDirectory {rootDirectory = r}

                       val () = chat ("auto-initialized package directory " ^ r)
                     in
                       Directory.mk {rootDirectory = r}
                     end
                   else
                     raise Error ("package directory does not exist: " ^ r)
                 end

             val () = rdir := SOME dir
           in
             dir
           end
    end;

(* ------------------------------------------------------------------------- *)
(* The directory package finder and importer.                                *)
(* ------------------------------------------------------------------------- *)

fun directoryFinder () = Directory.finder (directory ());

fun directoryImporter () = Directory.importer (directory ());

(* ------------------------------------------------------------------------- *)
(* Getting the latest version of packages.                                   *)
(* ------------------------------------------------------------------------- *)

fun latestVersionDirectory name =
    let
      val dir = directory ()
    in
      Directory.latestNameVersion dir name
    end;

fun getLatestVersionDirectory name =
    let
      val dir = directory ()
    in
      Directory.getLatestNameVersion dir name
    end;

(* ------------------------------------------------------------------------- *)
(* The directory staged package finder.                                      *)
(* ------------------------------------------------------------------------- *)

fun directoryStagedFinder () = Directory.stagedFinder (directory ());

(* ------------------------------------------------------------------------- *)
(* Config file.                                                              *)
(* ------------------------------------------------------------------------- *)

fun config () = Directory.config (directory ());

(* ------------------------------------------------------------------------- *)
(* System interface.                                                         *)
(* ------------------------------------------------------------------------- *)

fun system () = DirectoryConfig.system (config ());

(* ------------------------------------------------------------------------- *)
(* Package repo.                                                             *)
(* ------------------------------------------------------------------------- *)

local
  val repoOption : DirectoryRepo.name list ref = ref [];
in
  fun addRepository s =
      let
        val n = PackageName.fromString s

        val () = repoOption := !repoOption @ [n]
      in
        ()
      end;

  fun repository () =
      let
        val dir = directory ()

        val repos = Directory.repos dir

        val () =
            if not (List.null repos) then ()
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

        val ns = !repoOption
      in
        if List.null ns then repos
        else List.map (Directory.getRepo dir) ns
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Getting the latest version of packages on repos.                          *)
(* ------------------------------------------------------------------------- *)

fun latestNameVersionRepositories name chk' =
    let
      val repos = repositories ()
    in
      DirectoryRepo.latestNameVersionList repos name chk'
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
    "Given no arguments this command will clean up all staged theory packages.\n";

(* ------------------------------------------------------------------------- *)
(* Options for exporting installed theory packages.                          *)
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
    ArticleInfo
  | DocumentInfo
  | FilesInfo
  | FormatInfo of infoFormat
  | IncludesInfo
  | InferenceInfo
  | RequiresInfo
  | SummaryInfo
  | TagsInfo
  | TheoremsInfo
  | TheoryInfo;

fun savableInfo info =
    case info of
      ArticleInfo => true
    | _ => false;

fun mkInfoOutput info = (info,NONE);

fun defaultInfoOutputList inp =
    case inp of
      ArticleInput _ => [mkInfoOutput SummaryInfo]
    | PackageInput _ => [mkInfoOutput TagsInfo]
    | PackageNameInput _ => [mkInfoOutput TagsInfo]
    | PackageQueryInput _ => [mkInfoOutput TagsInfo]
    | StagedPackageInput _ => [mkInfoOutput TagsInfo]
    | TarballInput _ => [mkInfoOutput FilesInfo]
    | TheoryInput _ => [mkInfoOutput SummaryInfo];

val outputListInfo : (info * {filename : string} option) list ref = ref [];

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

fun addInfoOutput info =
    let
      val ref l = outputListInfo

      val () = outputListInfo := mkInfoOutput info :: l
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
          | (i,f) :: l =>
            case f of
              SOME {filename = f} =>
              raise Error ("multiple " ^ flag ^ " arguments:\n" ^
                           "  " ^ f ^ " and\n  " ^ filename)
            | NONE => (i, SOME {filename = filename}) :: l

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

  fun defaultize (i,f) = (i, Option.getOpt (f,defaultInfoOutputFilename));
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
            (fn _ => fn s =>
              addInfoOutput (FormatInfo (fromStringInfoFormat s)))},
       {switches = ["--information"], arguments = [],
        description = "display all package information",
        processor = beginOpt endOpt (fn _ => addInfoOutput TagsInfo)},
       {switches = ["--summary"], arguments = [],
        description = "display the package summary",
        processor = beginOpt endOpt (fn _ => addInfoOutput SummaryInfo)},
       {switches = ["--article"], arguments = [],
        description = "output the theory package in article format",
        processor = beginOpt endOpt (fn _ => addInfoOutput ArticleInfo)},
       {switches = ["--inference"], arguments = [],
        description = "display the number of primitive inferences",
        processor = beginOpt endOpt (fn _ => addInfoOutput InferenceInfo)},
       {switches = ["--theory"], arguments = [],
        description = "display the package theory file",
        processor = beginOpt endOpt (fn _ => addInfoOutput TheoryInfo)},
       {switches = ["--files"], arguments = [],
        description = "list the package files",
        processor = beginOpt endOpt (fn _ => addInfoOutput FilesInfo)},
       {switches = ["--includes"], arguments = [],
        description = "list the included theory packages",
        processor = beginOpt endOpt (fn _ => addInfoOutput IncludesInfo)},
       {switches = ["--requires"], arguments = [],
        description = "list satisfying required theory packages",
        processor = beginOpt endOpt (fn _ => addInfoOutput RequiresInfo)},
       {switches = ["--document"], arguments = [],
        description = "output the package document in HTML format",
        processor = beginOpt endOpt (fn _ => addInfoOutput DocumentInfo)},
       {switches = ["--theorems"], arguments = [],
        description = "output the package theorems in article format",
        processor = beginOpt endOpt (fn _ => addInfoOutput TheoremsInfo)},
       {switches = ["-o","--output"], arguments = ["FILE"],
        description = "write previous package information to FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn f => fn s => setInfoOutputFilename f s)},
       {switches = ["--show-assumptions"], arguments = [],
        description = "do not omit satisfied assumptions",
        processor = beginOpt endOpt (fn _ => showAssumptionsInfo := true)},
       {switches = ["--show-derivations"], arguments = [],
        description = "show the assumptions/axioms for each theorem",
        processor = beginOpt endOpt (fn _ => showDerivationsInfo := true)},
       {switches = ["--upgrade-theory"], arguments = [],
        description = "upgrade the theory file to the latest versions",
        processor = beginOpt endOpt (fn _ => upgradeTheoryInfo := true)},
       {switches = ["--preserve-theory"], arguments = [],
        description = "do not optimize the theory file",
        processor = beginOpt endOpt (fn _ => preserveTheoryInfo := true)}];
end;

val infoFooter =
    describeInputFormat ^ "\n" ^
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
      [{switches = ["--repo"], arguments = [],
        description = "configure the new package directory as a repo",
        processor =
          beginOpt endOpt
            (fn _ => repoInit := DirectoryConfig.repoDefault)}];
end;

val initFooter = "";

(* ------------------------------------------------------------------------- *)
(* Options for uninstalling theory packages.                                 *)
(* ------------------------------------------------------------------------- *)

val autoUninstall = ref false;

local
  open Useful Options;
in
  val uninstallOpts : opt list =
      [{switches = ["--auto"], arguments = [],
        description = "also uninstall included packages",
        processor = beginOpt endOpt (fn _ => autoUninstall := true)}];
end;

val uninstallFooter =
    describeNameFormat ^ ".\n" ^
    describeVersionFormat ^ ".\n";

(* ------------------------------------------------------------------------- *)
(* Options for installing theory packages.                                   *)
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
        description = "uninstall the package if it exists",
        processor = beginOpt endOpt (fn _ => reinstall := true)}] @
      List.map (addSuffix "-uninstall") uninstallOpts @
      [{switches = ["--manual"], arguments = [],
        description = "do not also install included packages",
        processor = beginOpt endOpt (fn _ => autoInstall := false)},
       {switches = ["--name"], arguments = ["NAME"],
        description = "confirm the package name",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s =>
                nameInstall := SOME (PackageNameVersion.fromString s))},
       {switches = ["--checksum"], arguments = ["CHECKSUM"],
        description = "confirm the package checksum",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => checksumInstall := SOME (Checksum.fromString s))},
       {switches = ["--stage"], arguments = [],
        description = "stage the package for installation",
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
        description = "set the output format",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => setFormatList (fromStringInfoFormat s))}];
end;

val listFooter =
    describeQueryFormat ^
    describeNameFormat ^ ".\n" ^
    describeVersionFormat ^ ".\n" ^
    describeInfoFormat ^ ".\n" ^
    "If the QUERY argument is missing the default Identity is used instead.\n";

(* ------------------------------------------------------------------------- *)
(* Options for updating package lists.                                       *)
(* ------------------------------------------------------------------------- *)

local
  open Useful Options;
in
  val updateOpts : opt list =
      [];
end;

val updateFooter = "";

(* ------------------------------------------------------------------------- *)
(* Options for uploading packages.                                           *)
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
    | Uninstall => " NAME-VERSION"
    | Update => ""
    | Upload => " NAME|NAME-VERSION ...";

fun commandDescription cmd =
    case cmd of
      Cleanup => "clean up theory packages staged for installation"
    | Export => "export an installed theory package from OpenTheory"
    | Help => "display help on all available commands"
    | Info => "extract information from theory packages and files"
    | Init => "initialize a new package directory"
    | Install => "install a package from a theory file or repo"
    | List => "list installed theory packages"
    | Uninstall => "uninstall an installed theory package"
    | Update => "update repo package lists"
    | Upload => "upload theory packages to a repo";

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
        description = "use this theory package directory",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => rootDirectoryOption := SOME s)},
       {switches = ["--repo"], arguments = ["REPO"],
        description = "use these theory package repos",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => addRepository s)}];
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
    "The list command takes a special QUERY input:\n" ^
    describeQueryFormat ^
    describeNameFormat ^ ".\n" ^
    describeVersionFormat ^ ".\n" ^
    describeFileFormat ^ ".\n" ^
    describeInfoFormat ^ ".\n" ^
    describeDirFormat ^ ".\n" ^
    describeRepoFormat ^ ".\n";
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
        ArticleInput _ => raise Error "cannot clean up an article"
      | PackageInput _ => raise Error "cannot clean up an installed package"
      | PackageNameInput _ => raise Error "cannot clean up a package name"
      | PackageQueryInput _ => raise Error "cannot clean up a package query"
      | StagedPackageInput namever => namever
      | TarballInput _ => raise Error "cannot clean up a tarball"
      | TheoryInput _ => raise Error "cannot clean up a theory file";
in
  fun cleanup nameverl =
      let
        val dir = directory ()

        val nameverl =
            if not (List.null nameverl) then List.map cleanupInput nameverl
            else
              let
                val namevers = Directory.listStaged dir {maxAge = NONE}
              in
                PackageNameVersionSet.toList namevers
              end

        val () = List.app (cleanupStagedPackage dir) nameverl
      in
        ()
      end
      handle Error err =>
        raise Error (err ^ "\ncleaning up failed");
end;

(* ------------------------------------------------------------------------- *)
(* Exporting installed theory packages.                                      *)
(* ------------------------------------------------------------------------- *)

local
  fun exportInput inp =
      case inp of
        ArticleInput _ => raise Error "cannot export an article"
      | PackageInput namever => namever
      | PackageNameInput name => getLatestVersionDirectory name
      | PackageQueryInput _ => raise Error "cannot export a package query"
      | StagedPackageInput _ => raise Error "cannot export a staged package"
      | TarballInput _ => raise Error "cannot export a tarball"
      | TheoryInput _ => raise Error "cannot export a theory file";
in
  fun export inp =
      let
        val dir = directory ()

        val namever = exportInput inp

        val name = PackageNameVersion.name namever
      in
        if PackageName.isHaskell name then Haskell.export dir namever
        else raise Error ("unknown export type: " ^ PackageName.toString name)
      end
      handle Error err =>
        raise Error (err ^ "\ntheory package export failed");
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
    val cacheInfo : PackageInfo.info cache = ref NONE;

    fun computeInfo () = NONE;
  in
    fun setInfo info = cacheInfo := SOME (SOME info);

    val getInfo = getCached cacheInfo computeInfo;
  end;

  local
    val cacheChecksum : Checksum.checksum cache = ref NONE;

    fun computeChecksum () =
        case getInfo () of
          NONE => NONE
        | SOME info =>
          let
            val dir = directory ()

            val namever = PackageInfo.nameVersion info
          in
            Directory.checksum dir namever
          end;
  in
    val getChecksum = getCached cacheChecksum computeChecksum;
  end;

  local
    val cachePackage : Package.package cache = ref NONE;

    fun computePackage () =
        case getInfo () of
          SOME info => SOME (PackageInfo.package info)
        | NONE => NONE;
  in
    fun setPackage pkg = cachePackage := SOME (SOME pkg);

    val getPackage = getCached cachePackage computePackage;
  end;

  local
    val cacheRequires : PackageName.name list cache = ref NONE;

    fun computeRequires () =
        case getPackage () of
          SOME pkg => SOME (Package.requires pkg)
        | NONE => NONE;
  in
    val getRequires = getCached cacheRequires computeRequires;
  end;

  local
    val cacheNameVersion : PackageNameVersion.nameVersion cache =
        ref NONE;

    fun computeNameVersion () =
        case getInfo () of
          SOME info => SOME (PackageInfo.nameVersion info)
        | NONE =>
          case getPackage () of
            SOME pkg => total Package.nameVersion pkg
          | NONE => NONE;
  in
    fun setNameVersion namever = cacheNameVersion := SOME (SOME namever);

    val getNameVersion = getCached cacheNameVersion computeNameVersion;
  end;

  local
    val cacheTags : PackageTag.tag list cache = ref NONE;

    fun computeTags () =
        case getPackage () of
          SOME pkg => SOME (Package.tags pkg)
        | NONE => NONE;
  in
    val getTags = getCached cacheTags computeTags;
  end;

  local
    val cacheDirectory : {directory : string} cache = ref NONE;

    fun computeDirectory () =
        case getInfo () of
          SOME info => SOME (PackageInfo.directory info)
        | NONE => NONE;
  in
    fun setDirectory dir = cacheDirectory := SOME (SOME dir);

    val getDirectory = getCached cacheDirectory computeDirectory;
  end;

  local
    val cacheTheories : PackageTheory.theory list cache =
        ref NONE;

    fun upgradeTheories pkg =
        let
          val dir = directory ()

          val pkg =
              case Directory.upgrade dir pkg of
                SOME p => p
              | NONE => raise Error "no theory file upgrade possible"
        in
          Package.theory pkg
        end;

    fun unwindTheories theories =
        if !preserveTheoryInfo then SOME theories
        else
          case getDirectory () of
            NONE => NONE
          | SOME {directory = dir} =>
            let
              val importer = directoryImporter ()

              val thys =
                  PackageDag.mk
                    {importer = importer,
                     directory = dir,
                     theories = theories}

              val thys = PackageDag.unwind thys
            in
              SOME (PackageDag.theories thys)
            end;

    fun computeTheories () =
        case getInfo () of
          SOME info =>
          let
            val pkg = PackageInfo.package info
          in
            if not (!upgradeTheoryInfo) then SOME (Package.theory pkg)
            else unwindTheories (upgradeTheories pkg)
          end
        | NONE =>
          case getPackage () of
            NONE => NONE
          | SOME pkg =>
            let
              val theories =
                  if not (!upgradeTheoryInfo) then Package.theory pkg
                  else upgradeTheories pkg
            in
              unwindTheories theories
            end;
  in
    fun setTheories thys = cacheTheories := SOME (SOME thys);

    val getTheories = getCached cacheTheories computeTheories;
  end;

  local
    val cacheFiles : {filename : string} list cache = ref NONE;

    fun computeFiles () =
        case getInfo () of
          SOME info =>
          let
            val files = PackageInfo.allFiles info

            val files = List.map (PackageInfo.joinDirectory info) files
          in
            SOME files
          end
        | NONE => NONE;
  in
    fun setFiles files = cacheFiles := SOME (SOME files);

    val getFiles = getCached cacheFiles computeFiles;
  end;

  local
    val cacheTheoryFile : {filename : string} cache = ref NONE;

    fun computeTheoryFile () =
        case getInfo () of
          SOME info => SOME (PackageInfo.theoryFile info)
        | NONE => NONE;
  in
    fun setTheoryFile file = cacheTheoryFile := SOME (SOME file);

    val getTheoryFile = getCached cacheTheoryFile computeTheoryFile;
  end;

  local
    val cacheTarball : {filename : string} cache = ref NONE;

    fun computeTarball () =
        case getInfo () of
          SOME info => SOME (PackageInfo.tarball info)
        | NONE => NONE;
  in
    fun setTarball file = cacheTarball := SOME (SOME file);

    val getTarball = getCached cacheTarball computeTarball;
  end;

  local
    val cacheSavable : bool cache = ref NONE;

    fun computeSavable () = NONE;
  in
    fun setSavable sav = cacheSavable := SOME (SOME sav);

    val getSavable = getCached cacheSavable computeSavable;
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
          | SOME theories =>
            case getSavable () of
              NONE => NONE
            | SOME sav =>
              let
                val importer = directoryImporter ()

                val graph = TheoryGraph.empty {savable = sav}

                val imps = TheorySet.empty

                val int = Interpretation.natural

                val (graph,env) =
                    TheoryGraph.importTheories importer graph
                      {directory = dir,
                       imports = imps,
                       interpretation = int,
                       theories = theories}

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
        case getInfo () of
          SOME info => SOME (PackageInfo.theorems info)
        | NONE =>
          case getNameVersion () of
            NONE => NONE
          | SOME nv =>
            case getThms () of
              NONE => NONE
            | SOME ths =>
              let
                val seqs = Sequents.fromThms ths

                val ths' =
                    PackageTheorems.Theorems'
                      {package = nv,
                       sequents = seqs}
              in
                SOME (PackageTheorems.mk ths')
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
            val dir = directory ()
          in
            Directory.requiresTheorems dir reqs
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
                val pkg =
                    case getPackage () of
                      SOME p => p
                    | NONE => raise Error "no package information available"

                val {description} = Package.description pkg
              in
                description
              end
            | EmptyItem =>
              let
                val pkg =
                    case getPackage () of
                      SOME p => p
                    | NONE => raise Error "no package information available"

                val empty = Package.emptyTheory pkg
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
        ArticleInfo =>
        let
          val art =
              case getArticle () of
                SOME a => a
              | NONE => raise Error "no article information available"

          val {filename} = file
        in
          Article.toTextFile {article = art, filename = filename}
        end
      | DocumentInfo =>
        let
          val pkg = getPackage ()

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
                | NONE => raise Error "no summary information available"

          val files =
              let
                fun unwrap file =
                    case file of
                      SOME {filename} => SOME filename
                    | NONE => NONE
              in
                {theory = unwrap (getTheoryFile ()),
                 tarball = unwrap (getTarball ())}
              end

          val tool = versionHtml

          val doc =
              PackageDocument.Document'
                {package = pkg,
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
          fun mk nv = PackageNameVersion.toString nv ^ "\n"

          val pkg =
              case getPackage () of
                SOME p => p
              | NONE => raise Error "no includes information available"

          val incs = Package.includes pkg

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
                  case Directory.previousNameVersion (directory ()) nv of
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
                      val info = Directory.get (directory ()) nv'

                      val th = PackageInfo.theorems info
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
              | NONE => raise Error "no summary information available"

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
              | NONE => raise Error "no summary information available"

          val grammar = infoSummaryGrammar ()

          val context =
              if !showAssumptionsInfo then Summary.NoContext
              else
                case getRequiresTheorems () of
                  NONE => Summary.NoContext
                | SOME ths => PackageTheorems.context sum ths

          val show =
              case getPackage () of
                SOME pkg => Package.show pkg
              | NONE => Show.default

          val {filename} = file
        in
          Summary.toTextFileWithGrammar grammar
            {context = context,
             show = show,
             summary = sum,
             filename = filename}
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
              case getTheorems () of
                SOME ths => ths
              | NONE => raise Error "no theorem information available"

          val {filename} = file
        in
          PackageTheorems.toTextFile {theorems = ths, filename = filename}
        end
      | TheoryInfo =>
        let
          val tags =
              case getTags () of
                SOME t => t
              | NONE => raise Error "no package information available"

          val theories =
              case getTheories () of
                SOME t => t
              | NONE => raise Error "no theory information available"

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
      in
        ()
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

        val () = setDirectory {directory = dir}

        val () = setArticle art
      in
        processInfoOutputList infs
      end;

  fun infoPackage namever infs =
      let
        val finder = directoryFinder ()
      in
        case PackageFinder.find finder namever of
          NONE =>
          let
            val err =
                "can't find package " ^ PackageNameVersion.toString namever
          in
            raise Error err
          end
        | SOME info =>
          let
            val () = setInfo info
          in
            processInfoOutputList infs
          end
      end;

  fun infoPackageName name infs =
      let
        val namever = getLatestVersionDirectory name
      in
        infoPackage namever infs
      end;

  fun infoStagedPackage namever infs =
      let
        val finder = directoryStagedFinder ()
      in
        case PackageFinder.find finder namever of
          NONE =>
          let
            val err =
                "can't find staged package " ^
                PackageNameVersion.toString namever
          in
            raise Error err
          end
        | SOME info =>
          let
            val () = setInfo info
          in
            processInfoOutputList infs
          end
      end;

  fun infoTarball {filename} infs =
      let
        val sys = system ()

        val PackageTarball.Contents {nameVersion,theoryFile,otherFiles} =
            PackageTarball.contents sys {filename = filename}

        val () = setTarball {filename = filename}

        val () = setNameVersion nameVersion

        val () = setTheoryFile theoryFile

        val () = setFiles (theoryFile :: otherFiles)
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
            List.map joinDir (Package.articles pkg) @
            List.map (joinDir o PackageExtra.filename) (Package.extraFiles pkg)

        val () = setDirectory {directory = dir}

        val () = setTheoryFile {filename = filename}

        val () = setPackage pkg

        val () = setFiles files
      in
        processInfoOutputList infs
      end;
in
  fun info inp =
      let
        val infs = readInfoOutputList inp
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
      val {directory = d, autoInit = _} = rootDirectory ()

      val () = initDirectory {rootDirectory = d}

      val () = chat ("initialized package directory " ^ d)
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Uninstalling theory packages.                                             *)
(* ------------------------------------------------------------------------- *)

local
  fun uninstallInput inp =
      case inp of
        ArticleInput _ => raise Error "cannot uninstall an article"
      | PackageInput namever => namever
      | PackageNameInput _ => raise Error "cannot uninstall a package name"
      | PackageQueryInput _ => raise Error "cannot uninstall a package query"
      | StagedPackageInput _ =>
        let
          val err = "cannot uninstall a staged package (use cleanup instead)"
        in
          raise Error err
        end
      | TarballInput _ => raise Error "cannot uninstall a tarball"
      | TheoryInput _ => raise Error "cannot uninstall a theory file";

  fun complain errs =
      if List.null errs then ()
      else
        let
          val s = DirectoryError.toStringList errs
        in
          if DirectoryError.existsFatal errs then raise Error s
          else chat ("package uninstall warnings:\n" ^ s)
        end;

  fun uninstallPackage auto dir namever =
      let
        val errs = Directory.checkUninstall dir namever

        val () = complain errs

        val () = Directory.uninstall dir namever

        val () =
            chat ((if auto then "auto-" else "") ^
                  "uninstalled package " ^ PackageNameVersion.toString namever)
      in
        ()
      end;
in
  fun uninstallAuto dir namever =
      let
        val errs = Directory.checkUninstall dir namever

        val errs =
            if not (!autoUninstall) then errs
            else
              let
                val (_,errs) = DirectoryError.removeInstalledUser errs
              in
                errs
              end

        val () = complain errs

        val () =
            if not (!autoUninstall) then ()
            else
              let
                val desc =
                    Directory.includedByRTC dir
                      (Directory.includedBy dir namever)

                val desc = List.rev (Directory.includeOrder dir desc)
              in
                List.app (uninstallPackage true dir) desc
              end

        val () = uninstallPackage false dir namever
      in
        ()
      end;

  fun uninstall inp =
      let
        val dir = directory ()

        val namever = uninstallInput inp
      in
        uninstallAuto dir namever
      end
      handle Error err =>
        raise Error (err ^ "\ntheory package uninstall failed");
end;

(* ------------------------------------------------------------------------- *)
(* Installing theory packages.                                               *)
(* ------------------------------------------------------------------------- *)

local
  fun installAuto master namever =
      case DirectoryRepo.peek master namever of
        NONE =>
        let
          val err =
              "package " ^ PackageNameVersion.toString namever ^
              " not found on " ^ DirectoryRepo.toString master
        in
          raise Error err
        end
      | SOME chk =>
        let
          val () = installAutoFind master namever chk
        in
          ()
        end

  and installAutoFind master namever chk =
      let
        val dir = directory ()
      in
        case Directory.checksum dir namever of
          SOME chk' =>
          if Checksum.equal chk' chk then ()
          else
            let
              val err =
                  "a package called " ^ PackageNameVersion.toString namever ^
                  " with a different checksum is already installed"
            in
              raise Error err
            end
        | NONE =>
          let
            val repos = repositories ()
          in
            case DirectoryRepo.find repos (namever,chk) of
              NONE =>
              let
                val err =
                    "package " ^ PackageNameVersion.toString namever ^
                    " with specific checksum not found on any repo"
              in
                raise Error err
              end
            | SOME repo => installAutoRepo master repo namever chk
          end
      end

  and installAutoRepo master repo namever chk =
      let
        val dir = directory ()

        val errs = Directory.checkStagePackage dir repo namever chk

        val () =
            if List.null errs then ()
            else
              let
                val s = DirectoryError.toStringList errs
              in
                if DirectoryError.existsFatal errs then raise Error s
                else chat ("package auto-install warnings:\n" ^ s)
              end

        val finder = installAutoFinder master

        val tool = {tool = versionHtml}

        val () = Directory.stagePackage dir finder repo namever chk tool

        val () = Directory.installStaged dir namever chk

        val () = chat ("auto-installed package " ^
                       PackageNameVersion.toString namever)
      in
        ()
      end

  and installAutoFinder master =
      let
        val dir = directory ()

        fun finder namever =
            let
              val () = installAuto master namever
            in
              Directory.peek dir namever
            end
      in
        PackageFinder.mk finder
      end;
in
  fun installAutoFree namever =
      let
        val dir = directory ()
      in
        if Directory.member namever dir then ()
        else
          let
            val repos = repositories ()
          in
            case DirectoryRepo.first repos namever of
              SOME (repo,chk) => installAutoRepo repo repo namever chk
            | NONE =>
              let
                val err =
                    "can't find package " ^
                    PackageNameVersion.toString namever ^
                    " in any repo"
              in
                raise Error err
              end
          end
      end;

  fun installAutoFinderFree () =
      let
        val dir = directory ()

        fun finder namever =
            let
              val () = installAutoFree namever
            in
              Directory.peek dir namever
            end
      in
        PackageFinder.mk finder
      end;

  fun installFinder master =
      if not (!autoInstall) then directoryFinder ()
      else installAutoFinder master;

  fun installFinderFree () =
      if not (!autoInstall) then directoryFinder ()
      else installAutoFinderFree ();

  fun installStagedFinderFree () =
      if not (!stageInstall) then installFinderFree ()
      else
        let
          val stagedFinder = directoryStagedFinder ()

          val finder = installFinderFree ()
        in
          PackageFinder.orelsef stagedFinder finder
        end;
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

        val dir = directory ()

        val info =
            case PackageFinder.find (directoryStagedFinder ()) namever of
              SOME info => info
            | NONE =>
              let
                val err =
                    "can't find staged package " ^
                    PackageNameVersion.toString namever
              in
                raise Error err
              end

        val chk = PackageInfo.checksumTarball info

        val () =
            case !checksumInstall of
              NONE => ()
            | SOME chk' =>
              if Checksum.equal chk' chk then ()
              else raise Error "package checksum does not match"

        val errs = Directory.checkInstallStaged dir namever chk

        val () =
            if List.null errs then ()
            else
              let
                val s = DirectoryError.toStringList errs
              in
                if DirectoryError.existsFatal errs then raise Error s
                else chat ("package install warnings:\n" ^ s)
              end

        val () = Directory.installStaged dir namever chk

        val () =
            chat ("installed staged package " ^
                  PackageNameVersion.toString namever)
      in
        ()
      end
      handle Error err =>
        raise Error (err ^ "\nstaged package install failed");

  fun installPackage namever =
      let
        val () =
            if not (Option.isSome (!nameInstall)) then ()
            else raise Error "can't specify name for a package install"

        val () =
            if not (!stageInstall) then ()
            else raise Error "can't stage a package install"

        val dir = directory ()

        val repos = repositories ()

        val (repo,chk) =
            case !checksumInstall of
              SOME chk =>
              (case DirectoryRepo.find repos (namever,chk) of
                 SOME repo => (repo,chk)
               | NONE =>
                 let
                   val err =
                       "can't find package " ^
                       PackageNameVersion.toString namever ^
                       " with specified checksum in any repo"
                 in
                   raise Error err
                 end)
            | NONE =>
              (case DirectoryRepo.first repos namever of
                 NONE =>
                 let
                   val err =
                       "can't find package " ^
                       PackageNameVersion.toString namever ^
                       " in any repo package list"
                 in
                   raise Error err
                 end
               | SOME repo_chk => repo_chk)

        val errs = Directory.checkStagePackage dir repo namever chk

        val errs =
            if not (!reinstall) then errs
            else
              let
                val (staged,errs) = DirectoryError.removeAlreadyStaged errs

                val () =
                    if staged then Directory.cleanupStaged dir namever else ()
              in
                errs
              end

        val (replace,errs) =
            if not (!reinstall) then (false,errs)
            else DirectoryError.removeAlreadyInstalled errs

        val () =
            if List.null errs then ()
            else
              let
                val s = DirectoryError.toStringList errs
              in
                if DirectoryError.existsFatal errs then raise Error s
                else chat ("package install warnings:\n" ^ s)
              end

        val () = if replace then uninstallAuto dir namever else ()

        val finder = installFinder repo

        val tool = {tool = versionHtml}

        val () = Directory.stagePackage dir finder repo namever chk tool

        val () = Directory.installStaged dir namever chk

        val () =
            chat ((if replace then "re" else "") ^ "installed package " ^
                  PackageNameVersion.toString namever)
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
      in
        case latestNameVersionRepositories name (!checksumInstall) of
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
        | SOME (_,namever,chk) =>
          case latestVersionDirectory name of
            NONE => installPackage namever
          | SOME namever' =>
            let
              val v = PackageNameVersion.version namever
              and v' = PackageNameVersion.version namever'

              val proceed =
                  case PackageVersion.compare (v,v') of
                    LESS => false
                  | EQUAL =>
                    let
                      val dir = directory ()

                      val chk' =
                          case Directory.checksum dir namever' of
                            SOME c => c
                          | NONE => raise Bug "installPackageName"
                    in
                      not (Checksum.equal chk' chk)
                    end
                  | GREATER => true
            in
              if proceed then installPackage namever
              else
                let
                  val msg =
                      "installed package " ^
                      PackageNameVersion.toString namever' ^
                      " is up to date"

                  val () = chat msg
                in
                  ()
                end
            end
      end
      handle Error err =>
        raise Error (err ^ "\npackage name install failed");

  fun installTarball tarFile =
      let
        val dir = directory ()

        val sys = Directory.system dir

        val chk = PackageTarball.checksum sys tarFile

        val () =
            case !checksumInstall of
              NONE => ()
            | SOME chk' =>
              if Checksum.equal chk' chk then ()
              else raise Error "tarball checksum does not match"

        val contents = PackageTarball.contents sys tarFile

        val PackageTarball.Contents {nameVersion = namever, ...} = contents

        val () =
            case !nameInstall of
              NONE => ()
            | SOME namever' =>
              if PackageNameVersion.equal namever' namever then ()
              else raise Error "tarball name does not match"

        val errs = Directory.checkStageTarball dir contents

        val errs =
            if not (!reinstall) then errs
            else
              let
                val (staged,errs) = DirectoryError.removeAlreadyStaged errs

                val () =
                    if staged then Directory.cleanupStaged dir namever else ()
              in
                errs
              end

        val (replace,errs) =
            if not (!reinstall) then (false,errs)
            else DirectoryError.removeAlreadyInstalled errs

        val () =
            if List.null errs then ()
            else
              let
                val s = DirectoryError.toStringList errs
              in
                if DirectoryError.existsFatal errs then raise Error s
                else chat ("package install warnings:\n" ^ s)
              end

        val () = if replace then uninstallAuto dir namever else ()

        val finder = installStagedFinderFree ()

        val tool = {tool = versionHtml}

        val () = Directory.stageTarball dir finder tarFile contents tool

        val () =
            if !stageInstall then ()
            else Directory.installStaged dir namever chk

        val () =
            let
              val verb =
                  if !stageInstall then
                    (if replace then "uninstalled and staged" else "staged")
                  else
                    (if replace then "reinstalled" else "installed")

              val mesg =
                  verb ^ " package " ^ PackageNameVersion.toString namever ^
                  " from tarball"
            in
              chat mesg
            end
      in
        ()
      end
      handle Error err =>
        raise Error (err ^ "\npackage install from tarball failed");

  fun installTheory filename =
      let
        val () =
            if not (Option.isSome (!checksumInstall)) then ()
            else raise Error "can't specify checksum for a theory file install"

        val () =
            if not (!stageInstall) then ()
            else raise Error "can't stage a package install from theory file"

        val dir = directory ()

        val pkg = Package.fromTextFile filename

        val namever = Package.nameVersion pkg

        val () =
            case !nameInstall of
              NONE => ()
            | SOME namever' =>
              if PackageNameVersion.equal namever' namever then ()
              else raise Error "theory name does not match"

        val srcDir =
            let
              val {filename = thyFile} = filename
            in
              {directory = OS.Path.dir thyFile}
            end

        val errs = Directory.checkStageTheory dir namever pkg

        val errs =
            if not (!reinstall) then errs
            else
              let
                val (staged,errs) = DirectoryError.removeAlreadyStaged errs

                val () =
                    if staged then Directory.cleanupStaged dir namever else ()
              in
                errs
              end

        val (replace,errs) =
            if not (!reinstall) then (false,errs)
            else DirectoryError.removeAlreadyInstalled errs

        val (pars,errs) =
            if not (!autoInstall) then ([],errs)
            else DirectoryError.removeUninstalledInclude errs

        val () =
            if List.null errs then ()
            else
              let
                val s = DirectoryError.toStringList errs
              in
                if DirectoryError.existsFatal errs then raise Error s
                else chat ("package install warnings:\n" ^ s)
              end

        val () = if replace then uninstallAuto dir namever else ()

        val () = List.app installAutoFree pars

        val tool = {tool = versionHtml}

        val chk = Directory.stageTheory dir namever pkg srcDir tool

        val () = Directory.installStaged dir namever chk

        val () =
            chat ((if replace then "re" else "") ^ "installed package " ^
                  PackageNameVersion.toString namever ^ " from theory file")
      in
        ()
      end
      handle Error err =>
        raise Error (err ^ "\npackage install from theory file failed");
in
  fun install inp =
      case inp of
        ArticleInput _ => raise Error "cannot install an article"
      | PackageInput namever => installPackage namever
      | PackageNameInput name => installPackageName name
      | PackageQueryInput name => raise Error "cannot install a package query"
      | StagedPackageInput namever => installStagedPackage namever
      | TarballInput file => installTarball file
      | TheoryInput file => installTheory file;
end;

(* ------------------------------------------------------------------------- *)
(* Listing installed packages.                                               *)
(* ------------------------------------------------------------------------- *)

fun sortList dir pkgs ord =
    case ord of
      AlphabeticalList => PackageNameVersionSet.toList pkgs
    | IncludeList => Directory.includeOrder dir pkgs
    | DependencyList => Directory.dependencyOrder dir pkgs
    | ReverseList ord => List.rev (sortList dir pkgs ord);

fun list query =
    let
      val query =
          case query of
            NONE => DirectoryQuery.Identity
          | SOME inp =>
            case inp of
              ArticleInput _ => raise Error "cannot list an article"
            | PackageInput namever =>
              DirectoryQuery.Constant (DirectoryQuery.NameVersion namever)
            | PackageNameInput name =>
              DirectoryQuery.Constant (DirectoryQuery.Name name)
            | PackageQueryInput query => query
            | StagedPackageInput _ =>
              raise Error "cannot list a staged package"
            | TarballInput _ => raise Error "cannot list a tarball"
            | TheoryInput _ => raise Error "cannot list a theory file"

      val dir = directory ()

      val repos = repositories ()

      val pkgs =
          if DirectoryQuery.ignoresInput query then PackageNameVersionSet.empty
          else Directory.latest dir

      val pkgs = DirectoryQuery.evaluate dir repos query pkgs

      val pkgs = sortList dir pkgs (orderList ());

      val InfoFormat items = getFormatList ()

      fun mk namever =
          let
            fun mkItem item =
                case item of
                  ChecksumItem =>
                  let
                    val chk =
                        case Directory.checksum dir namever of
                          SOME c => c
                        | NONE => raise Error "corrupt checksum"
                  in
                    Checksum.toString chk
                  end
                | DescriptionItem =>
                  let
                    val info =
                        case Directory.peek dir namever of
                          SOME i => i
                        | NONE => raise Error "corrupt installation"

                    val pkg = PackageInfo.package info

                    val {description} = Package.description pkg
                  in
                    description
                  end
                | EmptyItem =>
                  let
                    val info =
                        case Directory.peek dir namever of
                          SOME i => i
                        | NONE => raise Error "corrupt installation"

                    val empty = PackageInfo.emptyTheory info
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
                  end
          in
            String.concat (List.map mkItem items @ ["\n"])
          end
          handle Error err =>
            raise Error ("package " ^ PackageNameVersion.toString namever ^
                         ": " ^ err)

      val strm = Stream.map mk (Stream.fromList pkgs)

      val ref f = outputList
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
                     DirectoryRepo.toString repo)
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

local
  fun uploadInput inp =
      case inp of
        ArticleInput _ => raise Error "cannot upload an article"
      | PackageInput namever => namever
      | PackageNameInput name => getLatestVersionDirectory name
      | PackageQueryInput _ => raise Error "cannot upload a package query"
      | StagedPackageInput _ => raise Error "cannot upload a staged package"
      | TarballInput _ => raise Error "cannot upload a tarball"
      | TheoryInput _ => raise Error "cannot upload a theory file";

  fun computeSupport dir repo namevers =
      let
        fun notInDir nv = not (Directory.member nv dir)

        fun notInRepo nv = not (DirectoryRepo.member nv repo)

        val (unknown,namevers) = List.partition notInDir namevers

        val namevers = PackageNameVersionSet.fromList namevers

        val namevers =
            let
              val ancs =
                  case !setUpload of
                    ManualUpload =>
                    PackageNameVersionSet.empty
                  | SubtheoryUpload =>
                    Directory.subtheoriesRTC dir namevers

              val ancs = PackageNameVersionSet.filter notInRepo ancs
            in
              PackageNameVersionSet.union ancs namevers
            end

        val support =
            let
              val ancs = Directory.includesRTC dir namevers

              val ancs = PackageNameVersionSet.filter notInRepo ancs

              val ancs = PackageNameVersionSet.difference ancs namevers
            in
              Directory.dependencyOrder dir ancs
            end

        val namevers = unknown @ Directory.dependencyOrder dir namevers
      in
        (support,namevers)
      end;

  fun summarizeUpload dir repo support packages =
      let
        val mesg =
            Print.toString (Directory.ppUpload dir)
              {repo = repo,
               support = support,
               packages = packages}

        val () = chat mesg

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

  fun startUpload repo =
      let
        val upl = DirectoryRepo.startUpload repo

        val {url} = DirectoryRepo.urlUpload upl

        val mesg =
            "started upload to " ^
            DirectoryRepo.toString repo ^
            ":\n  " ^ url

        val () = chat mesg

        val () = TextIO.flushOut TextIO.stdOut
      in
        upl
      end;

  fun supportUpload dir repo upl namever =
      let
        val () = Directory.supportUpload dir upl namever

        val mesg =
            "installed support package " ^
            PackageNameVersion.toString namever ^
            " on " ^ DirectoryRepo.toString repo

        val () = chat mesg

        val () = TextIO.flushOut TextIO.stdOut
      in
        ()
      end;

  fun packageUpload dir repo upl namever =
      let
        val () = Directory.packageUpload dir upl namever

        val mesg =
            "uploaded package " ^
            PackageNameVersion.toString namever ^
            " to " ^ DirectoryRepo.toString repo

        val () = chat mesg

        val () = TextIO.flushOut TextIO.stdOut
      in
        ()
      end;

  fun finishUpload repo upl =
      let
        val () = DirectoryRepo.finishUpload upl

        val mesg =
            "finished upload to " ^ DirectoryRepo.toString repo ^
            ", sent author confirmation email"

        val () = chat mesg

        val () = TextIO.flushOut TextIO.stdOut
      in
        ()
      end;

  fun deleteUpload repo upl =
      let
        val () = DirectoryRepo.deleteUpload upl

        val mesg =
            "encountered error, so deleted upload to " ^
            DirectoryRepo.toString repo

        val () = chat mesg

        val () = TextIO.flushOut TextIO.stdOut
      in
        ()
      end;
in
  fun upload inps =
      let
        val dir = directory ()

        val repo = repository ()

        val namevers = List.map uploadInput inps

        val () = DirectoryRepo.update repo

        val (support,namevers) = computeSupport dir repo namevers

        val errs =
            Directory.checkUpload dir
              {repo = repo,
               support = support,
               packages = namevers}

        val () =
            if List.null errs then ()
            else
              let
                val s = DirectoryError.toStringList errs
              in
                if DirectoryError.existsFatal errs then raise Error s
                else chat ("package upload warnings:\n" ^ s)
              end

        val () = summarizeUpload dir repo support namevers

        val proceed = not (!confirmUpload) orelse askToConfirmUpload ()
      in
        if not proceed then ()
        else
          let
            val upl = startUpload repo

            val () =
                let
                  val () = List.app (supportUpload dir repo upl) support

                  val () = List.app (packageUpload dir repo upl) namevers

                  val () = finishUpload repo upl
                in
                  ()
                end
                handle Error err =>
                  let
                    val () =
                        deleteUpload repo upl
                        handle Error err' => raise Error (err ^ "\n" ^ err')
                  in
                    raise Error err
                  end
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
      | (Uninstall,[pkg]) => uninstall pkg
      | (Update,[]) => update ()
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
