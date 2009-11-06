(* ========================================================================= *)
(* OPENTHEORY TESTS                                                          *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
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

open Useful;

val time = Portable.time;

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun SAY s =
  print
  ("-------------------------------------" ^
   "-------------------------------------\n" ^ s ^ "\n\n");

fun printer p x = print (Print.toString p x ^ "\n\n");

fun printval p x = (printer p x; x);

(* ------------------------------------------------------------------------- *)
val () = SAY "Symbol table tests";
(* ------------------------------------------------------------------------- *)

val syms : Symbol.symbol list = [];

val mkConj = Syntax.mkConj syms
and mkDisj = Syntax.mkDisj syms
and mkForall = Syntax.mkForall syms
and mkImp = Syntax.mkImp syms
and mkNeg = Syntax.mkNeg syms
and termFalse = Syntax.termFalse syms
and termTrue = Syntax.termTrue syms;

(* ------------------------------------------------------------------------- *)
val () = SAY "Term tests";
(* ------------------------------------------------------------------------- *)

val ts =
    let
      val p = Var.mk (Name.mkGlobal "p", Type.bool)
      and q = Var.mk (Name.mkGlobal "q", Type.bool)

      val t1 = Term.mkAbs (p, Term.mkVar p)
      and t2 = Term.mkAbs (q, Term.mkVar p)
      and t3 = Term.mkAbs (q, Term.mkVar q)
    in
      [t1,t2,t3]
    end;

val ts' = printval (Print.ppList Term.pp) (sort Term.compare ts);

val ts'' = printval (Print.ppList Term.pp) (sort Term.alphaCompare ts);

val ts =
    let
      val p = Var.mk (Name.mkGlobal "p", Type.bool)
      and q = Var.mk (Name.mkGlobal "q", Type.bool)

      val t1 =
          Term.mkEq
            (Term.mkApp (Term.mkAbs (p, Term.mkVar p), termFalse),
             termFalse)
      and t2 =
          Term.mkEq
            (Term.mkApp (Term.mkAbs (q, Term.mkVar p), Term.mkVar q),
             Term.mkVar p)
      and t3 =
          Term.mkEq
            (Term.mkApp (Term.mkAbs (q, Term.mkVar q), termTrue),
             termTrue)
    in
      [t1,t2,t3]
    end;

val ts' = printval (Print.ppList Term.pp) (sort Term.compare ts);

val ts'' = printval (Print.ppList Term.pp) (sort Term.alphaCompare ts);

(* ------------------------------------------------------------------------- *)
val () = SAY "Substitution";
(* ------------------------------------------------------------------------- *)

val (tm,sub) =
    let
      val p = Var.mk (Name.mkGlobal "p", Type.bool)
      and q = Var.mk (Name.mkGlobal "q", Type.bool)

      val t1 = mkImp (Term.mkVar q, Term.mkVar p)
      val tm = Term.mkAbs (q,t1)

      val tySub = TypeSubst.emptyMap
      val tmSub = TermSubst.singletonTermMap (p, Term.mkVar q)
      val sub = TermSubst.mk (tySub,tmSub)
    in
      (tm,sub)
    end;

val _ = printval Term.pp tm;

val _ = printval TermSubst.pp sub;

val tm' = printval (Print.ppOption Term.pp) (TermSubst.subst sub tm);

val (tm,sub) =
    let
      val x = Var.mk (Name.mkGlobal "x", Type.bool)
      and y = Var.mk (Name.mkGlobal "y", Type.bool)

      val tx = Term.mkVar x
      and ty = Term.mkVar y

      val t0 = mkForall (y,ty)
      val t1 = mkConj (t0,ty)
      val tm = Term.mkAbs (x,t1)

      val tySub = TypeSubst.emptyMap
      val tmSub = TermSubst.singletonTermMap (y,tx)
      val sub = TermSubst.mk (tySub,tmSub)
    in
      (tm,sub)
    end;

val _ = printval Term.pp tm;

val _ = printval TermSubst.pp sub;

val tm' = printval (Print.ppOption Term.pp) (TermSubst.subst sub tm);

val (tm,sub) =
    let
      val ty = Type.alpha

      val x = Var.mk (Name.mkGlobal "x", ty)
      and x' = Var.mk (Name.mkGlobal "x'", ty)
      and s = Var.mk (Name.mkGlobal "s", Type.mkFun (ty,Type.bool))
      and t = Var.mk (Name.mkGlobal "t", Type.mkFun (ty,Type.bool))
      and u = Var.mk (Name.mkGlobal "u", Type.mkFun (ty,Type.bool))

      val sx = Term.mkApp (Term.mkVar s, Term.mkVar x)
      and sx' = Term.mkApp (Term.mkVar s, Term.mkVar x')
      and tx = Term.mkApp (Term.mkVar t, Term.mkVar x)
      and tx' = Term.mkApp (Term.mkVar t, Term.mkVar x')
      and ux = Term.mkApp (Term.mkVar u, Term.mkVar x)
      and ux' = Term.mkApp (Term.mkVar u, Term.mkVar x')

      val t0 = mkForall (x, mkDisj (mkNeg sx, tx))
      and t1 = mkDisj (mkConj (sx, mkNeg tx), mkConj (mkNeg sx, tx))
      and t2 = mkForall (x, mkDisj (mkNeg tx, ux))
      and t3 = mkDisj (mkConj (tx', mkNeg ux'), mkConj (mkNeg tx', ux'))

      val t4 = mkConj (mkConj (t0,t1), mkConj (t2,t3))
      val tm = Term.mkAbs (x',t4)

      val tySub = TypeSubst.emptyMap
      val tmSub = TermSubst.singletonTermMap (x, Term.mkVar x')
      val sub = TermSubst.mk (tySub,tmSub)
    in
      (tm,sub)
    end;

val _ = printval Term.pp tm;

val _ = printval TermSubst.pp sub;

val tm' = printval (Print.ppOption Term.pp) (TermSubst.subst sub tm);

(* ------------------------------------------------------------------------- *)
val () = SAY "Reading interpretations";
(* ------------------------------------------------------------------------- *)

val INTERPRETATION_DIR = "interpretations";

val holLightInt =
    Interpretation.fromTextFile
      {filename = INTERPRETATION_DIR ^ "/hol-light.int"};

val _ = printval Interpretation.pp holLightInt;

(* ------------------------------------------------------------------------- *)
val () = SAY "Compressing articles";
(* ------------------------------------------------------------------------- *)

val ARTICLE_DIR = "articles";

fun compress interpretation filename =
    let
      val () = print ("Compressing article \"" ^ filename ^ ".art\"\n")

      val inputFilename = ARTICLE_DIR ^ "/" ^ filename ^ ".art"

      val article =
          time Article.fromTextFile
            {savable = true,
             simulations = HolLight.simulations,
             known = Article.empty,
             interpretation = interpretation,
             filename = inputFilename}

      val outputFilename = filename ^ ".art"

      val () =
          time Article.toTextFile
            {article = article,
             filename = outputFilename}

      val () = print "\n"
    in
      ()
    end;

val () = compress Interpretation.natural "example1";

val () = compress holLightInt "bool";

val () = compress holLightInt "tactics";

(* ------------------------------------------------------------------------- *)
val () = SAY "Summarizing articles";
(* ------------------------------------------------------------------------- *)

fun summarize filename =
    let
      val () = print ("Summarizing article \"" ^ filename ^ ".art\"\n")

      val artFilename = filename ^ ".art"

      val art =
          time Article.fromTextFile
            {savable = false,
             known = Article.empty,
             simulations = HolLight.simulations,
             interpretation = Interpretation.natural,
             filename = artFilename}

      val ths = Article.saved art

      val sum = Summary.fromThmSet ths

      val sumFilename = filename ^ ".sum"

      val () =
          time Summary.toTextFile
            {summary = sum,
             filename = sumFilename}

      val () = print "\n"
    in
      ()
    end;

val () = summarize "example1";

val () = summarize "bool";

val () = summarize "tactics";

(* ------------------------------------------------------------------------- *)
val () = SAY "Compiling theories";
(* ------------------------------------------------------------------------- *)

val THEORY_DIR = "theories";

fun fromTextFilePackageTheory filename =
    PackageContents.theory (PackageContents.fromTextFile filename);

fun compile filename =
    let
      val () = print ("Compiling theory \"" ^ filename ^ ".thy\"\n")

      val thyFilename = THEORY_DIR ^ "/" ^ filename ^ ".thy"

      val thy = time fromTextFilePackageTheory {filename = thyFilename}

      val () = printer PackageTheory.pp thy

      val art =
          time Theory.toArticle
            {savable = true,
             known = Article.empty,
             simulations = HolLight.simulations,
             importToArticle = (fn _ => raise Bug "importToArticle"),
             interpretation = Interpretation.natural,
             directory = "",
             theory = thy}

      val artFilename = filename ^ ".art"

      val () =
          time Article.toTextFile
            {article = art,
             filename = artFilename}

      val () = print "\n"
    in
      ()
    end;

(* The simplest theory: empty *)

val () = compile "empty";

(* The next simplest theory: read one article *)

val () = compile "justBool";

val () = compile "justTactics";

(* Concatenating two articles *)

val () = compile "boolBool";

val () = compile "tacticsTactics";

val () = compile "boolTactics";

(* Localizing articles *)

val () = compile "localBoolTactics";
