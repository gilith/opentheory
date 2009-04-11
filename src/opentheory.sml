(* ========================================================================= *)
(* THE OPENTHEORY PROGRAM FOR MANIPULATING PROOF ARTICLES                    *)
(*                                                                           *)
(* Copyright (c) 2004-2009 Joe Hurd                                          *)
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

val PROGRAM = "opentheory";

(* ------------------------------------------------------------------------- *)
(* Commands.                                                                 *)
(* ------------------------------------------------------------------------- *)

datatype summary =
    SummaryText of {filename : string};

val COMPILE_OUTPUT = ref "-";

val SUMMARIZE_OUTPUT : summary list ref = ref [];

datatype command =
    Compile
  | Summarize;

val allCommands = [Compile,Summarize];

fun commandString cmd =
    case cmd of
      Compile => "compile"
    | Summarize => "summarize";

fun commandUsage cmd =
    case cmd of
      Compile => "input.thy"
    | Summarize => "input.art";

fun commandDescription cmd =
    case cmd of
      Compile => "compile a theory to an article"
    | Summarize => "summarize an article";

local
  open Useful Options;

  val compileOpts : opt list =
      [{switches = ["-o","--output"], arguments = ["FILE"],
        description = "write the compiled article to FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s => COMPILE_OUTPUT := s)}];

  val summarizeOpts : opt list =
      [{switches = ["--summary-text"], arguments = ["FILE"],
        description = "write the summary as text to FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s =>
             let
               val ref ss = SUMMARIZE_OUTPUT
               val ss = SummaryText {filename = s} :: ss
               val () = SUMMARIZE_OUTPUT := ss
             in
               ()
             end)}];
in
  fun commandOpts cmd =
      case cmd of
        Compile => compileOpts
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
  val globalOpts =
      [];
end;

val VERSION = "1.0";

val versionString = PROGRAM^" "^VERSION^" (release 20090401)"^"\n";

local
  fun mkProgramOptions header opts =
      {name = PROGRAM,
       version = versionString,
       header = "usage: "^PROGRAM^" "^header^"\n",
       footer = "Read from stdin or write to stdout using " ^
                "the special - filename.\n",
       options = opts @ Options.basicOptions};

  val globalUsage = "[global opts] command [command opts] input ...";

  val globalHeader =
      let
        fun f cmd =
            ["  " ^ PROGRAM ^ " " ^ commandString cmd ^ " ...",
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

fun compile {filename} =
    let
      val theory = Theory.fromTextFile {filename = filename}

      val article = Theory.toArticle theory

      val ref filename = COMPILE_OUTPUT
    in
      Article.toTextFile {article = article, filename = filename}
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

        val known = Article.new {savable = savable}

        val interpret = Interpretation.natural

        val article =
            Article.fromTextFile
              {savable = savable,
               known = known,
               interpretation = interpret,
               filename = filename}

        val summary = Article.summarize article

        val ref modes = SUMMARIZE_OUTPUT
      in
        List.app (outputSummary summary) (rev modes)
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
        (Compile,[filename]) => compile {filename = filename}
      | (Summarize,[filename]) => summarize {filename = filename}
      | _ => usage ("bad arguments for " ^ commandString cmd ^ " command")
in
  succeed ()
end
handle Error s => die (PROGRAM^" failed:\n" ^ s)
     | Bug s => die ("BUG found in "^PROGRAM^" program:\n" ^ s);
