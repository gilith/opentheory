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
(* The program name.                                                         *)
(* ------------------------------------------------------------------------- *)

val program = "opentheory";

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

val rootDirectory : string option ref = ref NONE;

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
                 case !rootDirectory of
                   SOME r => Directory.mk {rootDirectory = r}
                 | NONE =>
                   case OS.Process.getEnv homeEnvVar of
                     SOME d =>
                     let
                       val r = OS.Path.joinDirFile {dir = d, file = rootHomeDir}
                     in
                       if (OS.FileSys.isDir r handle OS.SysErr _ => false) then
                         Directory.mk {rootDirectory = r}
                       else
                         let
                           val () = chat ("Creating package directory " ^ r)
                         in
                           Directory.create {rootDirectory = r}
                         end
                     end
                   | NONE => raise Error "please specify the package directory"

             val () = rdir := SOME dir
           in
             dir
           end
    end;

fun finder () = Directory.lookup (directory ());

(* ------------------------------------------------------------------------- *)
(* Simulations.                                                              *)
(* ------------------------------------------------------------------------- *)

val defaultSimulations =
    Simulation.unionList
      [HolLight.simulations];

val simulations = ref defaultSimulations;

(* ------------------------------------------------------------------------- *)
(* Options for compiling packages to articles.                               *)
(* ------------------------------------------------------------------------- *)

datatype compileOutput =
    ArticleCompileOutput of {filename : string}
  | SummaryTextCompileOutput of {filename : string}
  | TheoryCompileOutput of {filename : string};

fun savableCompileOutput output =
    case output of
      ArticleCompileOutput _ => true
    | SummaryTextCompileOutput _ => false
    | TheoryCompileOutput _ => false;

val compileOutput : compileOutput list ref = ref [];

local
  open Useful Options;
in
  val compileOpts : opt list =
      [{switches = ["--article"], arguments = ["FILE"],
        description = "write the compiled article to FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s =>
             let
               val ref outs = compileOutput
               val outs = outs @ [ArticleCompileOutput {filename = s}]
               val () = compileOutput := outs
             in
               ()
             end)},
       {switches = ["--theory"], arguments = ["FILE"],
        description = "write the compiled theory to FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s =>
             let
               val ref outs = compileOutput
               val outs = outs @ [TheoryCompileOutput {filename = s}]
               val () = compileOutput := outs
             in
               ()
             end)},
       {switches = ["--summary-text"], arguments = ["FILE"],
        description = "write the summary as text to FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s =>
             let
               val ref outs = compileOutput
               val outs = outs @ [SummaryTextCompileOutput {filename = s}]
               val () = compileOutput := outs
             in
               ()
             end)}];
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
    PackageInfo
  | FileInfo
  | DepInfo;

val infoQuery = ref PackageInfo;

val infoOutput = ref "-";

local
  open Useful Options;
in
  val infoOpts : opt list =
      [{switches = ["--files"], arguments = [],
        description = "list the package files",
        processor =
          beginOpt endOpt
            (fn _ => infoQuery := FileInfo)},
       {switches = ["--deps"], arguments = [],
        description = "list the package dependencies",
        processor =
          beginOpt endOpt
            (fn _ => infoQuery := DepInfo)},
       {switches = ["-o","--output"], arguments = ["FILE"],
        description = "write the package information to FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => infoOutput := s)}];
end;

(* ------------------------------------------------------------------------- *)
(* Options for listing installed packages.                                   *)
(* ------------------------------------------------------------------------- *)

val listOutput = ref "-";

local
  open Useful Options;
in
  val listOpts : opt list =
      [{switches = ["-o","--output"], arguments = ["FILE"],
        description = "write the package list to FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => listOutput := s)}];
end;

(* ------------------------------------------------------------------------- *)
(* Commands.                                                                 *)
(* ------------------------------------------------------------------------- *)

datatype command =
    Compile
  | Help
  | Info
  | List;

val allCommands = [Compile,Help,Info,List];

fun commandString cmd =
    case cmd of
      Compile => "compile"
    | Help => "help"
    | Info => "info"
    | List => "list";

fun commandArgs cmd =
    case cmd of
      Compile => " input.thy"
    | Help => ""
    | Info => " <package-name>"
    | List => "";

fun commandDescription cmd =
    case cmd of
      Compile => "compile a theory package"
    | Help => "display command help"
    | Info => "display package information"
    | List => "list installed theory packages";

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

fun commandOpts cmd =
    case cmd of
      Compile => compileOpts
    | Help => helpOpts
    | Info => infoOpts
    | List => listOpts;

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
        description = "the package directory",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => rootDirectory := SOME s)}];
end;

val version = "1.0";

val versionString = program^" "^version^" (release 20100126)"^"\n";

local
  fun mkProgramOptions header opts =
      {name = program,
       version = versionString,
       header = "usage: "^program^" "^header^"\n",
       footer = "Read from stdin or write to stdout using " ^
                "the special - filename.\n",
       options = opts @ Options.basicOptions};

  val globalUsage = "[global opts] command [command opts] input ...";

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
        (commandString cmd ^ " [" ^ commandString cmd ^ " opts]" ^
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
(* Compiling packages to articles.                                           *)
(* ------------------------------------------------------------------------- *)

fun compile {filename} =
    let
      val ref outs = compileOutput

      val () =
          if not (null outs) then ()
          else usage "please specify a compilation target"

      val directory = OS.Path.dir filename
      val filename = OS.Path.file filename

      val pkg =
          Package.fromTextFile
            {name = NONE,
             directory = directory,
             filename = filename}

      val graph = Graph.empty
      and finder = finder ()
      and savable = List.exists savableCompileOutput outs
      and ref sim = simulations
      and imps = InstanceSet.empty
      and int = Interpretation.natural

      val (graph,inst) =
          Graph.importPackage graph
            {finder = finder,
             savable = savable,
             simulations = sim,
             imports = imps,
             interpretation = int,
             package = pkg}

      val art = Instance.article inst

      val sum = Instance.summary inst

      fun output out =
          case out of
            ArticleCompileOutput {filename} =>
            Article.toTextFile {article = art, filename = filename}
          | SummaryTextCompileOutput {filename} =>
            let
              val tags = Package.tags pkg

              val show = Show.fromTags tags
            in
              Summary.toTextFile
                {show = show,
                 summary = sum,
                 filename = filename}
            end
          | TheoryCompileOutput {filename} =>
            let
              val instReq = Graph.mkRequires (Graph.ancestors inst)

              fun instName inst =
                  case InstanceMap.peek instReq inst of
                    NONE => raise Bug "unknown theory import"
                  | SOME req => PackageRequire.name req

              val tags = []

              val reqs = InstanceMap.foldr (fn (_,r,l) => r :: l) [] instReq

              val thy = Theory.map instName (Instance.theory inst)

              val contents =
                  PackageContents.Contents
                    {tags = tags,
                     requires = reqs,
                     theory = thy}
            in
              PackageContents.toTextFile
                {contents = contents,
                 filename = filename}
            end

      val () = List.app output outs

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

(* ------------------------------------------------------------------------- *)
(* Displaying command help.                                                  *)
(* ------------------------------------------------------------------------- *)

fun help () = usage ("displaying command help");

(* ------------------------------------------------------------------------- *)
(* Displaying package information.                                           *)
(* ------------------------------------------------------------------------- *)

fun info name =
    let
      val find = finder ()

      val pkg = PackageName.fromString name
    in
      case PackageFinder.find find pkg of
        NONE => raise Error ("can't find package "^name)
      | SOME p =>
        let
          val s =
              case !infoQuery of
                PackageInfo =>
                let
                  val t = Package.tags p
                in
                  Print.toStream Tag.ppList t
                end
              | FileInfo =>
                let
                  val {directory = d} = Package.directory p

                  fun mk {filename = f} =
                      OS.Path.joinDirFile {dir = d, file = f} ^ "\n"

                  val fl = Package.filenames p
                in
                  Stream.map mk (Stream.fromList fl)
                end
              | DepInfo =>
                let
                  fun mk n = PackageName.toString n ^ "\n"

                  val pl = Package.dependencies p
                in
                  Stream.map mk (Stream.fromList pl)
                end

          val ref f = infoOutput
        in
          Stream.toTextFile {filename = f} s
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Listing installed packages.                                               *)
(* ------------------------------------------------------------------------- *)

fun list () =
    let
      val dir = directory ()

      val pkgs = PackageNameSet.toList (Directory.list dir)

      fun mk n = PackageName.toString n ^ "\n"

      val strm = Stream.map mk (Stream.fromList pkgs)

      val ref f = listOutput
    in
      Stream.toTextFile {filename = f} strm
    end;

(* ------------------------------------------------------------------------- *)
(* Top level.                                                                *)
(* ------------------------------------------------------------------------- *)

val () =
let
  (*BasicDebug val () = print "Running in basic DEBUG mode.\n" *)
  (*MetisDebug val () = print "Running in metis DEBUG mode.\n" *)
  (*OpenTheoryDebug val () = print "Running in opentheory DEBUG mode.\n" *)

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
        (Compile,[filename]) => compile {filename = filename}
      | (Help,[]) => help ()
      | (Info,[pkg]) => info pkg
      | (List,[]) => list ()
      | _ =>
        commandUsage cmd ("bad arguments for " ^ commandString cmd ^ " command")
in
  succeed ()
end
handle Error s => die (program^" failed:\n" ^ s)
     | Bug s => die ("BUG found in "^program^" program:\n" ^ s);
