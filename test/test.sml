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

val () = installPP pp_type;
val () = installPP pp_term;
val () = installPP pp_subst;
val () = installPP pp_thm;

val time = Portable.time;

(*DEBUG
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
val () = SAY "Reading in hol-light theories";
(* ------------------------------------------------------------------------- *)

(***
val q = ("q", bool_ty);
val T = mk_const ("T", bool_ty);
val qT = mk_abs (q,T);
val qTq = mk_comb (qT, mk_var q);
val u = TU.singleton (q,T);
TU.subst u (mk_var q);
TU.subst u qTq;
***)

val ARTICLE_DIR = "articles";

val (ths,art) =
    time
      Article.from_textfile
      {filename = ARTICLE_DIR ^ "/bool.art",
       translation = !Article.hol_light};

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
