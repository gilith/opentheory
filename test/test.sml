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
    TextIO.print
      ("-------------------------------------" ^
       "-------------------------------------\n" ^ s ^ "\n\n");

fun printer p x = TextIO.print (Print.toString p x ^ "\n\n");

fun printval p x = (printer p x; x);

(* ------------------------------------------------------------------------- *)
val () = SAY "Symbol tables";
(* ------------------------------------------------------------------------- *)

local
  val sym = Symbol.empty;
in
  val constForall = Syntax.constForall sym
  and mkConj = Syntax.mkConj sym
  and mkDisj = Syntax.mkDisj sym
  and mkForall = Syntax.mkForall sym
  and mkImp = Syntax.mkImp sym
  and mkNeg = Syntax.mkNeg sym
  and mkSelect = Syntax.mkSelect sym
  and termFalse = Syntax.termFalse sym
  and termTrue = Syntax.termTrue sym;
end;

(* ------------------------------------------------------------------------- *)
val () = SAY "Terms";
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

val (t1,t2) =
    let
      val b = Type.bool

      val bb = Type.mkFun (b,b)

      val bbb = Type.mkFun (bb,b)

      val bbbb = Type.mkFun (bbb,b)

      val f = Var.mk (Name.mkGlobal "f", bbb)
      and g = Var.mk (Name.mkGlobal "g", bb)
      and x = Var.mk (Name.mkGlobal "x", b)
      and y = Var.mk (Name.mkGlobal "y", b)

      val ft = Term.mkVar f
      and gt = Term.mkVar g
      and xt = Term.mkVar x
      and yt = Term.mkVar y

      val pat = Term.mkAbs (y, Term.mkApp (gt, mkConj (xt,yt)))

      val body = Term.mkApp (gt,xt)

      val geq = Term.mkEq (Term.mkApp (ft,pat), body)

      val t1 = mkSelect (f, mkForall (g, mkForall (x,geq)))

      val t2 = Term.mkApp (Term.mkConst (constForall,bbbb), t1)
    in
      (t1,t2)
    end;

val _ = printval Term.pp t1;

val _ = printval Term.pp t2;

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

fun mkInterpretationFilename name =
    let
      val file = OS.Path.joinBaseExt {base = name, ext = SOME "int"}
    in
      OS.Path.joinDirFile {dir = INTERPRETATION_DIR, file = file}
    end;

val holLightInt =
    Interpretation.fromTextFile
      {filename = mkInterpretationFilename "hol-light"};

val _ = printval Interpretation.pp holLightInt;

fun getInt s =
    if s = "hol-light" then holLightInt
    else if s = "natural" then Interpretation.natural
    else raise Error ("unknown interpretation: " ^ s);

(* ------------------------------------------------------------------------- *)
val () = SAY "Compressing articles";
(* ------------------------------------------------------------------------- *)

val ARTICLE_DIR = "articles";

fun mkSystemArticleFilename system name =
    let
      val dir = OS.Path.concat (ARTICLE_DIR, system)

      val file = OS.Path.joinBaseExt {base = name, ext = SOME "art"}
    in
      OS.Path.joinDirFile {dir = dir, file = file}
    end;

fun mkArticleFilename name =
    let
      val file = OS.Path.joinBaseExt {base = name, ext = SOME "art"}
    in
      OS.Path.joinDirFile {dir = ARTICLE_DIR, file = file}
    end;

fun compress system name =
    let
      val () = TextIO.print ("Compressing article \"" ^ name ^ "\"\n")

      val int = getInt system

      val inputFilename = mkSystemArticleFilename system name

      val article =
          time Article.fromTextFile
            {savable = true,
             import = Article.empty,
             interpretation = int,
             filename = inputFilename}

      val outputFilename = mkArticleFilename name

      val () =
          time Article.toTextFile
            {article = article,
             filename = outputFilename}

      val () = TextIO.print "\n"
    in
      ()
    end;

val () = compress "natural" "empty";

val () = compress "natural" "example1";

val () = compress "natural" "example2";

val () = compress "natural" "example3";

val () = compress "hol-light" "bool-def-true";

val () = compress "hol-light" "bool-int-true";

val () = compress "hol-light" "bool-true-aux";

(* ------------------------------------------------------------------------- *)
val () = SAY "Summarizing articles";
(* ------------------------------------------------------------------------- *)

fun mkSummaryFilename name =
    let
      val file = OS.Path.joinBaseExt {base = name, ext = SOME "sum"}
    in
      OS.Path.joinDirFile {dir = ARTICLE_DIR, file = file}
    end;

fun summarize name =
    let
      val () = TextIO.print ("Summarizing compressed article \"" ^ name ^ "\"\n")

      val artFilename = mkArticleFilename name

      val art =
          Article.fromTextFile
            {savable = false,
             import = Article.empty,
             interpretation = Interpretation.natural,
             filename = artFilename}

      val ths = Article.thms art

      val sum = Summary.fromThms ths

      val () = printer Summary.pp sum
    in
      ()
    end;

val () = summarize "empty";

val () = summarize "example1";

val () = summarize "example2";

val () = summarize "example3";

val () = summarize "bool-def-true";

val () = summarize "bool-int-true";

val () = summarize "bool-true-aux";

(* ------------------------------------------------------------------------- *)
val () = SAY "Theory package directories";
(* ------------------------------------------------------------------------- *)

val DIRECTORY_DIR = "directory";

val directory =
    printval Directory.pp (Directory.mk {rootDirectory = DIRECTORY_DIR});

(* ------------------------------------------------------------------------- *)
val () = SAY "Config files";
(* ------------------------------------------------------------------------- *)

val config = printval DirectoryConfig.pp (Directory.config directory);

(* ------------------------------------------------------------------------- *)
val () = SAY "Importing theory packages";
(* ------------------------------------------------------------------------- *)

fun import name =
    let
      val () = TextIO.print ("Importing theory package \"" ^ name ^ "\"\n")

      val finder = Directory.finder directory

      val graph = Graph.empty {savable = false}

      val spec =
          Graph.Specification
            {imports = TheorySet.empty,
             interpretation = Interpretation.natural,
             name = PackageName.fromString name}

      val (_,thy) = Graph.importPackageName finder graph spec

      val art = Theory.article thy

      val ths = Article.thms art

      val sum = Summary.fromThms ths

      val () = printer Summary.pp sum
    in
      ()
    end;

(* The simplest package: empty *)

val () = import "empty-1.0";

(* HOL Light definition of the constant T *)

val () = import "bool-def-true-1.0";

(* HOL Light theorem about T *)

val () = import "bool-int-true-1.0";

(* Compilation of the definition and theorem *)

val () = import "bool-true-1.0";
