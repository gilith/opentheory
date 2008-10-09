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

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun SAY s =
  print
  ("-------------------------------------" ^
   "-------------------------------------\n" ^ s ^ "\n\n");

fun printval p x = (print (Print.toString p x ^ "\n\n"); x);

(* ------------------------------------------------------------------------- *)
val () = SAY "Term tests";
(* ------------------------------------------------------------------------- *)

val ts =
    let
      val p = (Name.mkGlobal "p", boolTy)
      and q = (Name.mkGlobal "q", boolTy)

      val t1 = mkAbs (p, mkVar p)
      and t2 = mkAbs (q, mkVar p)
      and t3 = mkAbs (q, mkVar q)
    in
      [t1,t2,t3]
    end;

val ts' = printval (Print.ppList ppTerm) (sort Term.compare ts);

val ts'' = printval (Print.ppList ppTerm) (sort Term.alphaCompare ts);

val ts =
    let
      val p = (Name.mkGlobal "p", boolTy)
      and q = (Name.mkGlobal "q", boolTy)

      val t1 = mkEq (mkComb (mkAbs (p, mkVar p), falseTm), falseTm)
      and t2 = mkEq (mkComb (mkAbs (q, mkVar p), mkVar q), mkVar p)
      and t3 = mkEq (mkComb (mkAbs (q, mkVar q), trueTm), trueTm)
    in
      [t1,t2,t3]
    end;

val ts' = printval (Print.ppList ppTerm) (sort Term.compare ts);

val ts'' = printval (Print.ppList ppTerm) (sort Term.alphaCompare ts);

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

val ARTICLE_DIR = "articles";

fun compress interpretation filename =
    let
      val article =
          time
            Article.fromTextFile
            {savable = true,
             known = ThmSet.empty,
             interpretation = interpretation,
             filename = ARTICLE_DIR ^ "/" ^ filename};
    in
      time (Article.toTextFile {filename = filename}) article
    end;

val () = compress holLightInt "bool.art";

val known = ThmSet.empty;

val bool =
    time
      Article.fromTextFile
      {savable = false,
       known = known,
       interpretation = Interpretation.natural,
       filename = "bool.art"};

val summary =
    withRef (showHyp,false) (printval Summary.pp) (Article.summarize bool);
