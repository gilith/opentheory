(* ========================================================================= *)
(* THE OPENTHEORY PROGRAM FOR MANIPULATING PROOF ARTICLES                    *)
(*                                                                           *)
(* Copyright (c) 2004-2008 Joe Hurd                                          *)
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
(* Program options.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype command =
    ResetInterpretation
  | ReadInterpretation of {filename : string}
  | ReadArticle of {filename : string}
  | OutputArticle of {filename : string}
  | OutputSummary of {filename : string};

val COMMANDS : command list ref = ref [];

fun isOutputArticle cmd =
    case cmd of
      OutputArticle _ => true
    | _ => false;

local
  open Useful Options;
in
  val specialOptions =
      [{switches = ["-i","--interpret"], arguments = ["FILE"],
        description = "interpret articles using the rules in FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s =>
             let
               val ref cs = COMMANDS
               val cs = ResetInterpretation :: cs
               val cs = ReadInterpretation {filename = s} :: cs
               val () = COMMANDS := cs
             in
               ()
             end)},
       {switches = ["-a","--article"], arguments = ["FILE"],
        description = "read the article in FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s =>
             let
               val ref cs = COMMANDS
               val cs = ReadArticle {filename = s} :: cs
               val () = COMMANDS := cs
             in
               ()
             end)},
       {switches = ["-o","--output"], arguments = ["FILE"],
        description = "write the article to FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s =>
             let
               val ref cs = COMMANDS
               val cs = OutputArticle {filename = s} :: cs
               val () = COMMANDS := cs
             in
               ()
             end)},
       {switches = ["--summary"], arguments = ["FILE"],
        description = "write the article summary to FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s =>
             let
               val ref cs = COMMANDS
               val cs = OutputSummary {filename = s} :: cs
               val () = COMMANDS := cs
             in
               ()
             end)},
       {switches = ["--add-interpret"], arguments = ["FILE"],
        description = "add the interpretation rules in FILE",
        processor =
          beginOpt (stringOpt endOpt)
            (fn _ => fn s =>
             let
               val ref cs = COMMANDS
               val cs = ReadInterpretation {filename = s} :: cs
               val () = COMMANDS := cs
             in
               ()
             end)}];
end;

val VERSION = "1.0";

val versionString = PROGRAM^" "^VERSION^" (release 20090401)"^"\n";

val programOptions =
    {name = PROGRAM,
     version = versionString,
     header = "usage: "^PROGRAM^" [option ...] article.art ...\n" ^
              "Interprets and concatenates proof articles.\n",
     footer = "Read from standard input or write to standard output using\n" ^
              "the special - filename.\n",
     options = specialOptions @ Options.basicOptions};

fun exit x : unit = Options.exit programOptions x;
fun succeed () = Options.succeed programOptions;
fun fail mesg = Options.fail programOptions mesg;
fun usage mesg = Options.usage programOptions mesg;

val (opts,work) =
    Options.processOptions programOptions (CommandLine.arguments ());

val () = if null work then () else usage "bad argument format";

(* ------------------------------------------------------------------------- *)
(* The core application.                                                     *)
(* ------------------------------------------------------------------------- *)

fun processCommand (cmd,(interpret,article)) =
    case cmd of
      ResetInterpretation =>
      let
        val interpret = Interpretation.natural
      in
        (interpret,article)
      end
    | ReadInterpretation filename =>
      let
        val interpret' = Interpretation.fromTextFile filename

        val interpret = Interpretation.append interpret interpret'
      in
        (interpret,article)
      end
    | ReadArticle {filename} =>
      let
        val article =
            Article.appendTextFile
              {interpretation = interpret,
               filename = filename}
              article
      in
        (interpret,article)
      end
    | OutputArticle {filename} =>
      let
        val () =
            Article.toTextFile
              {article = article,
               filename = filename}
      in
        (interpret,article)
      end
    | OutputSummary {filename} =>
      let
        val summary = Article.summarize article

        val () =
            Summary.toTextFile
              {summary = summary,
               filename = filename}
      in
        (interpret,article)
      end;

(* ------------------------------------------------------------------------- *)
(* Top level.                                                                *)
(* ------------------------------------------------------------------------- *)

val () =
let
  (*BasicDebug val () = print "Running in basic DEBUG mode.\n" *)
  (*MetisDebug val () = print "Running in metis DEBUG mode.\n" *)
  (*OpenTheoryDebug val () = print "Running in opentheory DEBUG mode.\n" *)

  val commands = rev (!COMMANDS)

  val interpret = Interpretation.natural

  val article = Article.new {savable = List.exists isOutputArticle commands}

  val (_,article) = List.foldl processCommand (interpret,article) commands
in
  succeed ()
end
handle Error s => die (PROGRAM^" failed:\n" ^ s)
     | Bug s => die ("BUG found in "^PROGRAM^" program:\n" ^ s);
