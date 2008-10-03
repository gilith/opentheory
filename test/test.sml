(* ========================================================================= *)
(* OPEN THEORY TESTS                                                         *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* Dummy versions of Moscow ML declarations to stop real compilers barfing.  *)
(* ------------------------------------------------------------------------- *)

(*mlton
val quotation = ref true;
val quietdec  = ref true;
val loadPath  = ref ([] : string list);
val load = fn (_ : string) => ();
*)

(*polyml
val quotation = ref true;
val quietdec  = ref true;
val loadPath  = ref ([] : string list);
val load = fn (_ : string) => ();
*)

(* ------------------------------------------------------------------------- *)
(* Load and open some useful modules                                         *)
(* ------------------------------------------------------------------------- *)

val () = loadPath := !loadPath @ ["../bin/mosml"];
val () = app load ["Options"];

open Useful Syntax Rule;

val time = Portable.time;

(*OpenTheoryDebug
  val () = print "Running in DEBUG mode.\n"
*)

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun SAY s =
  print
  ("-------------------------------------" ^
   "-------------------------------------\n" ^ s ^ "\n\n");

fun printval p x = (print (Print.toString p x ^ "\n\n"); x);

(* ------------------------------------------------------------------------- *)
val () = SAY "Reading in the hol-light interpretation";
(* ------------------------------------------------------------------------- *)

val INTERPRETATION_DIR = "interpretations";

val holLightInt =
    Interpretation.fromTextFile
      {filename = INTERPRETATION_DIR ^ "/hol-light.int"};

val () = print (Interpretation.toString holLightInt);

(* ------------------------------------------------------------------------- *)
val () = SAY "Reading in hol-light theories";
(* ------------------------------------------------------------------------- *)

val ARTICLE_DIR = "articles/hol-light";

(***
val ARTICLES =
    ["preamble.art",
     "nets.art",
     "basics.art",
     "printer.art",
     "preterm.art",
     "parser.art",
     "equal.art",
     "tactics.art",
     "itab.art",
     "drule.art",
     "bool.art",
     "simp.art",
     "theorems.art",
     "ind-defs.art",
     "class.art",
     "trivia.art",
     "canon.art",
     "meson.art",
     "recursion.art",
     "quot.art",
     "pair.art",
     "num.art",
     "arith.art",
     "wf.art",
     "calc_num.art",
     "normalizer.art",
     "grobner.art",
     "ind-types.art",
     "list.art",
     "realax.art",
     "calc_int.art",
     "realarith.art",
     "real.art",
     "calc_rat.art",
     "int.art",
     "sets.art",
     "iter.art",
     "cart.art",
     "define.art",
     "help.art",
     "database.art"];

fun read filename known =
    let
      val () = print (filename ^ ":\n")
      val art =
          time
            Article.fromTextFile
            {known = known,
             interpretation = holLightInt,
             filename = ARTICLE_DIR ^ "/" ^ filename}
      val thms = Article.saved art
      val known = ThmSet.union known thms
    in
      (thms,known)
    end;

val articles = maps read ARTICLES ThmSet.empty;

stop;
***)

val known = ThmSet.empty;

(***
val bool =
    time
      Article.fromTextFile
      {known = known,
       interpretation = holLightInt,
       filename = ARTICLE_DIR ^ "/bool.art"};

val known = ThmSet.union known (Article.saved bool);
***)

val num =
    time
      Article.fromTextFile
      {known = known,
       interpretation = holLightInt,
       filename = ARTICLE_DIR ^ "/num.art"};

val filename = "compressed.art";

val () = time (Article.toTextFile {filename = filename}) num;

val num' =
    time
      Article.fromTextFile
      {known = known,
       interpretation = Interpretation.natural,
       filename = filename};

val summary = printval Summary.pp (Article.summarize num');
