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

val INTERPRETATIONS : string list ref = ref [];

val INCLUDES : string list ref = ref [];

val OUTPUT : string option ref = ref NONE;

local
  open Useful Options;
in
  val specialOptions =
    [{switches = ["-i","--interpret"], arguments = ["FILE"],
      description = "interpret articles using the rules in FILE",
      processor =
        beginOpt (stringOpt endOpt)
          (fn _ => fn s =>
           let val r as ref ss = INTERPRETATIONS in r := s :: ss end)},
     {switches = ["-I","--include"], arguments = ["FILE"],
      description = "use the saved theorems in FILE",
      processor =
        beginOpt (stringOpt endOpt)
          (fn _ => fn s =>
           let val r as ref ss = INCLUDES in r := s :: ss end)},
     {switches = ["-o","--output"], arguments = ["FILE"],
      description = "output the processed article to FILE",
      processor =
        beginOpt (stringOpt endOpt) (fn _ => fn s => OUTPUT := SOME s)}];
end;

val VERSION = "1.0";

val versionString = "OpenTheory "^VERSION^" (release 20080816)"^"\n";

val programOptions =
    {name = PROGRAM,
     version = versionString,
     header = "usage: "^PROGRAM^" [option ...] article.art ...\n" ^
              "Processes the input proof articles.\n",
     footer = "Articles can be read from standard input or written to " ^
              "standard output using the special - filename.\n",
     options = specialOptions @ Options.basicOptions};

fun exit x : unit = Options.exit programOptions x;
fun succeed () = Options.succeed programOptions;
fun fail mesg = Options.fail programOptions mesg;
fun usage mesg = Options.usage programOptions mesg;

val (opts,work) =
    Options.processOptions programOptions (CommandLine.arguments ());

val () = if null work then usage "no input proof articles" else ();

(* ------------------------------------------------------------------------- *)
(* The core application.                                                     *)
(* ------------------------------------------------------------------------- *)

val mkInterpretation =
    let
      fun add (filename,int) =
          Interpretation.append int
            (Interpretation.fromTextFile {filename = filename})
    in
      List.foldl add Interpretation.natural
    end;

fun readArticle interpretation (filename,(article,known)) =
    let
      val article' =
          Article.fromTextFile
            {known = known,
             interpretation = interpretation,
             filename = filename}

      val article = Article.append article article'

      val known = ThmSet.union known (Article.saved article')
    in
      (article,known)
    end;

fun includeArticle (filename,known) =
    let
      val interpretation = Interpretation.natural
      val article = Article.empty
      val (_,known) = readArticle interpretation (filename,(article,known))
    in
      known
    end;

(* ------------------------------------------------------------------------- *)
(* Top level.                                                                *)
(* ------------------------------------------------------------------------- *)

val () =
let
  (*BasicDebug val () = print "Running in basic DEBUG mode.\n" *)
  (*MetisDebug val () = print "Running in metis DEBUG mode.\n" *)
  (*OpenTheoryDebug val () = print "Running in opentheory DEBUG mode.\n" *)

  val interpretation = mkInterpretation (rev (!INTERPRETATIONS))

  val known = ThmSet.empty

  val known = foldl includeArticle known (rev (!INCLUDES))

  val article = Article.empty

  val (article,_) = foldl (readArticle interpretation) (article,known) work

  val () =
      case !OUTPUT of
        SOME filename =>
        Article.toTextFile {filename = filename} article
      | NONE =>
        Summary.toTextFile {filename = "-"} (Article.summarize article)
in
  succeed ()
end
handle Error s => die (PROGRAM^" failed:\n" ^ s)
     | Bug s => die ("BUG found in "^PROGRAM^" program:\n" ^ s);
