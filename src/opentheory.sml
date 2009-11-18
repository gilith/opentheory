(* ========================================================================= *)
(* THE OPENTHEORY PROGRAM FOR MANIPULATING PROOF ARTICLES                    *)
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
(* The program name.                                                         *)
(* ------------------------------------------------------------------------- *)

val program = "opentheory";

(* ------------------------------------------------------------------------- *)
(* Simulations.                                                              *)
(* ------------------------------------------------------------------------- *)

val defaultSimulations =
    Simulation.unionList
      [HolLight.simulations];

(* ------------------------------------------------------------------------- *)
(* Commands.                                                                 *)
(* ------------------------------------------------------------------------- *)

datatype info =
    PackageInfo
  | FileInfo;

datatype summary =
    SummaryText of {filename : string};

val simulations = ref defaultSimulations;

val rootDirectory : string option ref = ref NONE;

val infoQuery = ref PackageInfo;

val infoOutput = ref "-";

val compileOutput = ref "-";

val summarizeOutput : summary list ref = ref [];

datatype command =
    Info
  | Compile
  | Summarize;

val allCommands = [Info,Compile,Summarize];

fun commandString cmd =
    case cmd of
      Info => "info"
    | Compile => "compile"
    | Summarize => "summarize";

fun commandUsage cmd =
    case cmd of
      Info => "<package-name>"
    | Compile => "input.thy"
    | Summarize => "input.art";

fun commandDescription cmd =
    case cmd of
      Info => "display package information"
    | Compile => "compile a theory package to an article"
    | Summarize => "summarize an article";

local
  open Useful Options;

  val infoOpts : opt list =
      [{switches = ["-f","--files"], arguments = [],
        description = "list the package files",
        processor =
          beginOpt endOpt
            (fn _ => infoQuery := FileInfo)},
       {switches = ["-o","--output"], arguments = ["FILE"],
        description = "write the package information to FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => infoOutput := s)}];

  val compileOpts : opt list =
      [{switches = ["-o","--output"], arguments = ["FILE"],
        description = "write the compiled article to FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => compileOutput := s)}];

  val summarizeOpts : opt list =
      [{switches = ["--summary-text"], arguments = ["FILE"],
        description = "write the summary as text to FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s =>
             let
               val ref ss = summarizeOutput
               val ss = SummaryText {filename = s} :: ss
               val () = summarizeOutput := ss
             in
               ()
             end)}];
in
  fun commandOpts cmd =
      case cmd of
        Info => infoOpts
      | Compile => compileOpts
      | Summarize => summarizeOpts;
end;

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
      fun mk cmd =
          let
            val s = commandString cmd

            fun f {switches,arguments,description,processor} =
                {switches = switches,
                 arguments = arguments,
                 description = "(" ^ s ^ ") " ^ description,
                 processor = processor}
          in
            map f (commandOpts cmd)
          end
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

val versionString = program^" "^version^" (release 20090717)"^"\n";

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
        (commandString cmd ^ " [" ^ commandString cmd ^ " opts] " ^
         commandUsage cmd ^ "\n" ^
         capitalize (commandDescription cmd) ^ ".\n" ^
         "Displaying " ^ commandString cmd ^ " options:")
        (commandOpts cmd);

  val programOptions =
      mkProgramOptions
        (globalHeader ^ "Displaying all options:")
        (globalOpts @ allCommandOptions);
end;

fun exit x : unit = Options.exit programOptions x;
fun succeed () = Options.succeed programOptions;
fun fail mesg = Options.fail programOptions mesg;
fun usage mesg = Options.usage programOptions mesg;

(* ------------------------------------------------------------------------- *)
(* The core application.                                                     *)
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
                 case !rootDirectory of
                   SOME d => Directory.mk {rootDirectory = d}
                 | NONE => raise Error "specify the package directory"

             val () = rdir := SOME dir
           in
             dir
           end
    end;

fun finder () = Directory.lookup (directory ());

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

(*OpenTheoryTrace1
                  fun ppFilename {filename} = Print.ppString filename
                  val () = Print.trace (Print.ppList ppFilename) "fl" fl
*)
                in
                  Stream.fromList (map mk fl)
                end

          val ref f = infoOutput
        in
          Stream.toTextFile {filename = f} s
        end
    end;

fun compile {filename} =
    let
      val directory = OS.Path.dir filename
      val filename = OS.Path.file filename

      val pkg =
          Package.fromTextFile
            {name = NONE,
             directory = directory,
             filename = filename}

      val graph = Graph.empty
      and finder = PackageFinder.useless
      and savable = true
      and ref sim = simulations
      and req = InstanceSet.empty
      and int = Interpretation.natural

      val (graph,inst) =
          Graph.importPackage graph
            {finder = finder,
             savable = savable,
             simulations = sim,
             requires = req,
             interpretation = int,
             package = pkg}

      val art = Instance.article inst

      val ref filename = compileOutput
    in
      Article.toTextFile {article = art, filename = filename}
    end;

local
  fun outputSummary summary mode =
      case mode of
        SummaryText {filename} =>
        Summary.toTextFile {summary = summary, filename = filename};
in
  fun summarize {filename} =
      let
        val savable = false
        and known = Article.empty
        and ref sim = simulations
        and int = Interpretation.natural

        val art =
            Article.fromTextFile
              {savable = savable,
               known = known,
               simulations = sim,
               interpretation = int,
               filename = filename}

        val ths = Article.saved art

        val sum = Summary.fromThmSet ths

        val ref modes = summarizeOutput
      in
        List.app (outputSummary sum) (rev modes)
      end;
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
        (Info,[pkg]) => info pkg
      | (Compile,[filename]) => compile {filename = filename}
      | (Summarize,[filename]) => summarize {filename = filename}
      | _ => usage ("bad arguments for " ^ commandString cmd ^ " command")
in
  succeed ()
end
handle Error s => die (program^" failed:\n" ^ s)
     | Bug s => die ("BUG found in "^program^" program:\n" ^ s);
