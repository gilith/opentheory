(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYNTAX                                                 *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Syntax :> Syntax =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Primitive.                                                                *)
(* ------------------------------------------------------------------------- *)

type name = Name.name;
type ty = Type.ty;
type var = Var.var;
type term = Term.term;
type sequent = Sequent.sequent;
type thm = Thm.thm;

(* Type variables *)

val mkTypeVar = Type.mkVar;
val destTypeVar = Type.destVar;
val isTypeVar = Type.isVar;
val equalTypeVar = Type.equalVar;

val alphaType = Type.alpha;

(* Type operators *)

val mkTypeOp = Type.mkOp;
val destTypeOp = Type.destOp;
val isTypeOp = Type.isOp;

(* The type of booleans *)

val boolType = Type.bool;

(* Function types *)

val mkFun = Type.mkFun;
val destFun = Type.destFun;
val isFun = Type.isFun;

fun listMkFun ([],ty) = ty
  | listMkFun (x :: xs, ty) = mkFun (x, listMkFun (xs,ty));

local
  fun strip acc ty =
    if not (isFun ty) then (rev acc, ty)
    else let val (x,ty) = destFun ty in strip (x :: acc) ty end;
in
  val stripFun = strip [];
end;

(* Constants *)

val mkConst = Term.mkConst;
val destConst = Term.destConst;
val isConst = Term.isConst;

(* Variables *)

val mkVar = Term.mkVar;
val destVar = Term.destVar;
val isVar = Term.isVar;
val equalVar = Term.equalVar;

(* Function applications *)

val mkComb = Term.mkComb;
val destComb = Term.destComb;
val isComb = Term.isComb;

val rator = fst o destComb;

val rand = snd o destComb;

val land = rand o rator;

fun listMkComb (tm,[]) = tm
  | listMkComb (tm, x :: xs) = listMkComb (mkComb (tm,x), xs);

local
  fun strip acc tm =
    if not (isComb tm) then (tm,acc)
    else let val (tm,x) = destComb tm in strip (x :: acc) tm end;
in
  val stripComb = strip [];
end;

(* Lambda abstractions *)

val absString = "\\";

val mkAbs = Term.mkAbs;
val destAbs = Term.destAbs;
val isAbs = Term.isAbs;

fun listMkAbs ([],tm) = tm
  | listMkAbs (v :: vs, tm) = mkAbs (v, listMkAbs (vs,tm));

local
  fun strip acc tm =
    if not (isAbs tm) then (rev acc, tm)
    else let val (v,tm) = destAbs tm in strip (v :: acc) tm end;
in
  val stripAbs = strip [];
end;

(* Equality *)

val eqType = Term.eqType;
val eqTerm = Term.eqTerm;
val mkEq = Term.mkEq;
val destEq = Term.destEq;
val isEq = Term.isEq;
val lhs = fst o destEq;
val rhs = snd o destEq;

(* Theorems *)

fun axioms th = case Thm.dest th of Thm.Thm {axioms,...} => axioms;

fun sequent th = case Thm.dest th of Thm.Thm {sequent,...} => sequent;

fun hyp th = case Thm.dest th of Thm.Thm {sequent = {hyp, ...}, ...} => hyp;

fun concl th =
    case Thm.dest th of Thm.Thm {sequent = {concl, ...}, ...} => concl;

(* ------------------------------------------------------------------------- *)
(* Operators.                                                                *)
(* ------------------------------------------------------------------------- *)

(* Unary operators *)

fun mkUnop n (ty,a) =
    let
      val c = mkConst (n,ty)
    in
      mkComb (c,a)
    end;

fun destUnop n tm =
    let
      val (c,a) = destComb tm
      val (n',ty) = destConst c
      val _ = Name.equal n n' orelse raise Error "Syntax.destUnop"
    in
      (ty,a)
    end;

fun isUnop n = can (destUnop n);

(* Binary operators *)

fun mkBinop n (ty,a,b) =
    let
      val c = mkConst (n,ty)
      val t = mkComb (c,a)
    in
      mkComb (t,b)
    end;

fun destBinop n tm =
    let
      val (t,b) = destComb tm
      val (c,a) = destComb t
      val (n',ty) = destConst c
      val _ = Name.equal n n' orelse raise Error "Syntax.destBinop"
    in
      (ty,a,b)
    end;

fun isBinop n = can (destBinop n);

(* ------------------------------------------------------------------------- *)
(* Boolean.                                                                  *)
(* ------------------------------------------------------------------------- *)

(* True *)

val trueString = "T";

val trueName = Name.mkGlobal trueString;

val trueTerm = mkConst (trueName,boolType);

fun isTrue tm = Term.equal tm trueTerm;

(* False *)

val falseString = "F";

val falseName = Name.mkGlobal falseString;

val falseTerm = mkConst (falseName,boolType);

fun isFalse tm = Term.equal tm falseTerm;

(* Negations *)

val negString = "~";

val negName = Name.mkGlobal negString;

val mkNeg =
    let
      val negTy = mkFun (boolType,boolType)
    in
      fn tm => mkUnop negName (negTy,tm)
    end;

fun destNeg tm =
    let
      val (_,a) = destUnop negName tm
    in
      a
    end;

val isNeg = can destNeg;

(* Implications *)

val impName = Name.mkGlobal "==>";

val mkImp =
    let
      val impTy = mkFun (boolType, mkFun (boolType,boolType))
    in
      fn (a,b) => mkBinop impName (impTy,a,b)
    end;

fun destImp tm =
    let
      val (_,a,b) = destBinop impName tm
    in
      (a,b)
    end;

val isImp = can destImp;

(* Conjunctions *)

val conjName = Name.mkGlobal "/\\";

val mkConj =
    let
      val conjTy = mkFun (boolType, mkFun (boolType,boolType))
    in
      fn (a,b) => mkBinop conjName (conjTy,a,b)
    end;

fun destConj tm =
    let
      val (_,a,b) = destBinop conjName tm
    in
      (a,b)
    end;

val isConj = can destConj;

fun listMkConj tms =
    case rev tms of
      [] => trueTerm
    | tm :: tms => List.foldl mkConj tm tms;

local
  fun strip acc tm =
      case total destConj tm of
        NONE => List.revAppend (acc,[tm])
      | SOME (a,b) => strip (a :: acc) b;
in
  fun stripConj tm = if isTrue tm then [] else strip [] tm;
end;

(* Disjunctions *)

val disjName = Name.mkGlobal "\\/";

val mkDisj =
    let
      val disjTy = mkFun (boolType, mkFun (boolType,boolType))
    in
      fn (a,b) => mkBinop disjName (disjTy,a,b)
    end;

fun destDisj tm =
    let
      val (_,a,b) = destBinop disjName tm
    in
      (a,b)
    end;

val isDisj = can destDisj;

fun listMkDisj tms =
    case rev tms of
      [] => trueTerm
    | tm :: tms => List.foldl mkDisj tm tms;

local
  fun strip acc tm =
      case total destDisj tm of
        NONE => List.revAppend (acc,[tm])
      | SOME (a,b) => strip (a :: acc) b;
in
  fun stripDisj tm = if isTrue tm then [] else strip [] tm;
end;

(* Universal quantifiers *)

val forallString = "!";

val forallName = Name.mkGlobal forallString;

fun forallType a = mkFun (mkFun (a, boolType), boolType);

fun mkForall (v,b) =
    let
      val vTy = Var.typeOf v
    in
      mkComb (mkConst (forallName, forallType vTy), mkAbs (v,b))
    end;

fun destForall tm =
    let
      val (c,t) = destComb tm
      val _ = Name.equal (fst (destConst c)) forallName orelse
              raise Error "destForall"
    in
      destAbs t
    end;

val isForall = can destForall;

fun listMkForall ([],tm) = tm
  | listMkForall (v :: vs, tm) = mkForall (v, listMkForall (vs,tm));

local
  fun strip acc tm =
    if not (isForall tm) then (rev acc, tm)
    else let val (v,tm) = destForall tm in strip (v :: acc) tm end;
in
  val stripForall = strip [];
end;

(* Existential quantifiers *)

val existsString = "?";

val existsName = Name.mkGlobal existsString;

fun existsType a = mkFun (mkFun (a, boolType), boolType);

fun mkExists (v,b) =
    let
      val vTy = Var.typeOf v
    in
      mkComb (mkConst (existsName, existsType vTy), mkAbs (v,b))
    end;

fun destExists tm =
    let
      val (c,t) = destComb tm
      val _ = Name.equal (fst (destConst c)) existsName orelse
              raise Error "destExists"
    in
      destAbs t
    end;

val isExists = can destExists;

fun listMkExists ([],tm) = tm
  | listMkExists (v :: vs, tm) = mkExists (v, listMkExists (vs,tm));

local
  fun strip acc tm =
    if not (isExists tm) then (rev acc, tm)
    else let val (v,tm) = destExists tm in strip (v :: acc) tm end;
in
  val stripExists = strip [];
end;

(* Unique existential quantifiers *)

val existsUniqueString = "?!";

val existsUniqueName = Name.mkGlobal existsUniqueString;

fun existsUniqueType a = mkFun (mkFun (a, boolType), boolType);

fun mkExistsUnique (v,b) =
    let
      val vTy = Var.typeOf v
    in
      mkComb (mkConst (existsUniqueName, existsUniqueType vTy), mkAbs (v,b))
    end;

fun destExistsUnique tm =
    let
      val (c,t) = destComb tm
      val _ = Name.equal (fst (destConst c)) existsUniqueName orelse
              raise Error "destExistsUnique"
    in
      destAbs t
    end;

val isExistsUnique = can destExistsUnique;

fun listMkExistsUnique ([],tm) = tm
  | listMkExistsUnique (v :: vs, tm) =
    mkExistsUnique (v, listMkExistsUnique (vs,tm));

local
  fun strip acc tm =
      if not (isExistsUnique tm) then (rev acc, tm)
      else let val (v,tm) = destExistsUnique tm in strip (v :: acc) tm end;
in
  val stripExistsUnique = strip [];
end;

(* Hilbert's indefinite choice operator (epsilon) *)

fun selectType a = mkFun (mkFun (a, boolType), a);

val selectString = "select";

val selectName = Name.mkGlobal selectString;

val selectTerm =
    let
      val ty = selectType alphaType
    in
      mkConst (selectName,ty)
    end;

fun mkSelect (v_b as (v,b)) =
    let
      val vTy = Var.typeOf v
    in
      mkComb (mkConst (selectName, selectType vTy), mkAbs v_b)
    end;

fun destSelect tm =
    let
      val (_,t) = destUnop selectName tm
    in
      destAbs t
    end;

val isSelect = can destSelect;

fun listMkSelect ([],tm) = tm
  | listMkSelect (v :: vs, tm) = mkSelect (v, listMkSelect (vs,tm));

local
  fun strip acc tm =
    if not (isSelect tm) then (rev acc, tm)
    else let val (v,tm) = destSelect tm in strip (v :: acc) tm end;
in
  val stripSelect = strip [];
end;

(* ------------------------------------------------------------------------- *)
(* The type of individuals.                                                  *)
(* ------------------------------------------------------------------------- *)

val indName = Name.mkGlobal "ind";

val indArity = 0;

val indType = mkTypeOp (indName,[]);

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val typeMaximumSize = Type.maximumSize;
val varShowTypes = Var.showTypes;
val termMaximumSize = Term.maximumSize;
val termShowTypes = Term.showTypes;
val sequentShowHyp = Sequent.showHyp;
val thmShowHyp = Thm.showHyp;

val ppType = Type.pp;
val typeToString = Type.toString;

val ppVar = Var.pp;
val varToString = Var.toString;

val ppTerm = Term.pp;
val termToString = Term.toString;

val ppTypeSubstMap = TypeSubst.ppMap;
val typeSubstMapToString = TypeSubst.toStringMap;

val ppTypeSubst = TypeSubst.pp;
val typeSubstToString = TypeSubst.toString;

val ppTermSubstMap = TermSubst.ppTermMap;
val termSubstMapToString = TermSubst.toStringTermMap;

val ppSubstMap = TermSubst.ppMap;
val substMapToString = TermSubst.toStringMap;

val ppSubst = TermSubst.pp;
val substToString = TermSubst.toString;

val ppSequent = Sequent.pp;
val sequentToString = Sequent.toString;

val ppThm = Thm.pp;
val thmToString = Thm.toString;

end
