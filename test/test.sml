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
      val ty = alphaType

      val x = Var.Var (Name.mkGlobal "x", ty)
      and x' = Var.Var (Name.mkGlobal "x'", ty)
      and s = Var.Var (Name.mkGlobal "s", mkFun (ty,boolType))
      and t = Var.Var (Name.mkGlobal "t", mkFun (ty,boolType))
      and u = Var.Var (Name.mkGlobal "u", mkFun (ty,boolType))

      val t2 = mkConj (mkForall (x, mkDisj (mkNeg (mkComb (mkVar s, mkVar x)),
                                            mkComb (mkVar t, mkVar x))),
                       mkDisj (mkConj (mkComb (mkVar s, mkVar x),
                               mkNeg (mkComb (mkVar t, mkVar x))),
                       mkConj (mkNeg (mkComb (mkVar s, mkVar x)),
                               mkComb (mkVar t, mkVar x))))
      val t3 = mkForall (x, mkDisj (mkNeg (mkComb (mkVar t, mkVar x)),
                                    mkComb (mkVar u, mkVar x)))
      val t4 = mkDisj (mkConj (mkComb (mkVar t, mkVar x'),
                               mkNeg (mkComb (mkVar u, mkVar x'))),
                       mkConj (mkNeg (mkComb (mkVar t, mkVar x')),
                               mkComb (mkVar u, mkVar x')))
      val t1 = mkExists (x', listMkConj [t2,t3,t4])
      val t5 = mkComb (mkAbs (x,t1), mkVar x)
      val tm = mkEq (t5,t1)

      val tySub = TypeSubst.emptyMap
      val tmSub = TermSubst.singletonTermMap (x, mkVar x')
      val sub = TermSubst.mk (tySub,tmSub)
    in
      (tm,sub)
    end;

val _ = printval ppTerm tm;

val _ = printval ppSubst sub;

val tm' = printval (Print.ppOption ppTerm) (TermSubst.subst sub tm);

val th = axiom {hyp = TermAlphaSet.empty, concl = tm};

val _ = printval (Print.ppPair ppSubst ppThm) (sub,th);

val th' = printval ppThm (Thm.subst sub th);

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
    withRef (thmShowHyp,false) (printval Summary.pp) (Article.summarize bool);
