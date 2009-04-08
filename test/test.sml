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
      val p = Var.Var (Name.mkGlobal "p", boolType)
      and q = Var.Var (Name.mkGlobal "q", boolType)

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
      val p = Var.Var (Name.mkGlobal "p", boolType)
      and q = Var.Var (Name.mkGlobal "q", boolType)

      val t1 = mkEq (mkComb (mkAbs (p, mkVar p), falseTerm), falseTerm)
      and t2 = mkEq (mkComb (mkAbs (q, mkVar p), mkVar q), mkVar p)
      and t3 = mkEq (mkComb (mkAbs (q, mkVar q), trueTerm), trueTerm)
    in
      [t1,t2,t3]
    end;

val ts' = printval (Print.ppList ppTerm) (sort Term.compare ts);

val ts'' = printval (Print.ppList ppTerm) (sort Term.alphaCompare ts);

(* ------------------------------------------------------------------------- *)
val () = SAY "Substitution";
(* ------------------------------------------------------------------------- *)

val (tm,sub) =
    let
      val p = Var.Var (Name.mkGlobal "p", boolType)
      and q = Var.Var (Name.mkGlobal "q", boolType)

      val t1 = mkImp (mkVar q, mkVar p)
      val tm = mkAbs (q,t1)

      val tySub = TypeSubst.emptyMap
      val tmSub = TermSubst.singletonTermMap (p, mkVar q)
      val sub = TermSubst.mk (tySub,tmSub)
    in
      (tm,sub)
    end;

val _ = printval ppTerm tm;

val _ = printval ppSubst sub;

val tm' = printval (Print.ppOption ppTerm) (TermSubst.subst sub tm);

val (tm,sub) =
    let
      val x = Var.Var (Name.mkGlobal "x", boolType)
      and y = Var.Var (Name.mkGlobal "y", boolType)

      val tx = mkVar x
      and ty = mkVar y

      val t0 = mkForall (y,ty)
      val t1 = mkConj (t0,ty)
      val tm = mkAbs (x,t1)

      val tySub = TypeSubst.emptyMap
      val tmSub = TermSubst.singletonTermMap (y,tx)
      val sub = TermSubst.mk (tySub,tmSub)
    in
      (tm,sub)
    end;

val _ = printval ppTerm tm;

val _ = printval ppSubst sub;

val tm' = printval (Print.ppOption ppTerm) (TermSubst.subst sub tm);

val (tm,sub) =
    let
      val ty = alphaType

      val x = Var.Var (Name.mkGlobal "x", ty)
      and x' = Var.Var (Name.mkGlobal "x'", ty)
      and s = Var.Var (Name.mkGlobal "s", mkFun (ty,boolType))
      and t = Var.Var (Name.mkGlobal "t", mkFun (ty,boolType))
      and u = Var.Var (Name.mkGlobal "u", mkFun (ty,boolType))

      val sx = mkComb (mkVar s, mkVar x)
      and sx' = mkComb (mkVar s, mkVar x')
      and tx = mkComb (mkVar t, mkVar x)
      and tx' = mkComb (mkVar t, mkVar x')
      and ux = mkComb (mkVar u, mkVar x)
      and ux' = mkComb (mkVar u, mkVar x')

      val t0 = mkForall (x, mkDisj (mkNeg sx, tx))
      and t1 = mkDisj (mkConj (sx, mkNeg tx), mkConj (mkNeg sx, tx))
      and t2 = mkForall (x, mkDisj (mkNeg tx, ux))
      and t3 = mkDisj (mkConj (tx', mkNeg ux'), mkConj (mkNeg tx', ux'))

      val t4 = mkConj (mkConj (t0,t1), mkConj (t2,t3))
      val tm = mkAbs (x',t4)

      val tySub = TypeSubst.emptyMap
      val tmSub = TermSubst.singletonTermMap (x, mkVar x')
      val sub = TermSubst.mk (tySub,tmSub)
    in
      (tm,sub)
    end;

val _ = printval ppTerm tm;

val _ = printval ppSubst sub;

val tm' = printval (Print.ppOption ppTerm) (TermSubst.subst sub tm);

(* ------------------------------------------------------------------------- *)
val () = SAY "Reading in the hol-light interpretation";
(* ------------------------------------------------------------------------- *)

val INTERPRETATION_DIR = "interpretations";

val holLightInt =
    Interpretation.fromTextFile
      {filename = INTERPRETATION_DIR ^ "/hol-light.int"};

val _ = printval Interpretation.pp holLightInt;

(* ------------------------------------------------------------------------- *)
val () = SAY "Reading in hol-light articles";
(* ------------------------------------------------------------------------- *)

val ARTICLE_DIR = "articles";

fun compress interpretation filename =
    let
      val article =
          time Article.fromTextFile
            {savable = true,
             interpretation = interpretation,
             filename = ARTICLE_DIR ^ "/" ^ filename}
    in
      time Article.toTextFile {article = article, filename = filename}
    end;

val () = compress holLightInt "bool.art";

val bool =
    time Article.fromTextFile
      {savable = false,
       interpretation = Interpretation.natural,
       filename = "bool.art"};

val summary =
    withRef (thmShowHyp,false) (printval Summary.pp) (Article.summarize bool);

(* ------------------------------------------------------------------------- *)
val () = SAY "Reading in theories";
(* ------------------------------------------------------------------------- *)

val THEORY_DIR = "theories";

val testThy = time Theory.fromTextFile {filename = THEORY_DIR ^ "/test.thy"};

val _ = printval Theory.pp testThy

val testArt = time Theory.toArticle testThy;
