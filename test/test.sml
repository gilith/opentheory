(* ========================================================================= *)
(* OPEN THEORY TESTS                                                         *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* Dummy versions of Moscow ML declarations to stop MLton barfing.           *)
(* ------------------------------------------------------------------------- *)

(*mlton
val quotation = ref true;
val quietdec  = ref true;
val loadPath  = ref ([] : string list);
val load = fn (_ : string) => ();
val installPP = fn (_ : 'a Parser.pp) => ();
*)

(* ------------------------------------------------------------------------- *)
(* Load and open some useful modules                                         *)
(* ------------------------------------------------------------------------- *)

val () = loadPath := !loadPath @ ["../bin/mosml"];
val () = app load ["Options"];

open Useful Syntax Rule;

val () = installPP ppType;
val () = installPP ppTerm;
val () = installPP ppSubst;
val () = installPP ppThm;

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

fun printval p x = (print (Parser.toString p x ^ "\n\n"); x);

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

(***
val q = ("q", boolTy);
val T = mkConst ("T", boolTy);
val qT = mkAbs (q,T);
val qTq = mkComb (qT, mkVar q);
val u = TU.singleton (q,T);
TU.subst u (mkVar q);
TU.subst u qTq;
***)

val ARTICLE_DIR = "articles/hol-light";

val art =
    time
      Article.fromTextFile
      {filename = ARTICLE_DIR ^ "/bool.art",
       interpretation = holLightInt};

val filename = "compressed-bool.art";

val () =
    time
      Article.toTextFile
      {filename = filename,
       article = art};

val art' =
    time
      Article.fromTextFile
      {filename = filename,
       interpretation = Interpretation.natural};

(***
[
(*
   "preamble",
   "basics",
   "nets",
   "preterm",
   "parser",
   "printer",
   "equal",
   "bool",
   "drule",
   "tactics",
   "itab",
   "simp",
   "theorems",
   "ind-defs",
   "class",
   "trivia",
   "canon",
   "meson",
   "quot",
   "recursion",
   "pair",
*)
   "num"
(*
 "arith",
 "wf",
 "calc_num",
 "normalizer",
 "ind-types",
 "list",
 "realax",
 "real",
 "calc_rat",
 "int",
 "sets"
*)
];
***)
