(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYNTAX                                                 *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Syntax :> Syntax =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Operators.                                                                *)
(* ------------------------------------------------------------------------- *)

(* Nullary operators *)

fun mkNullaryOp n ty = Term.mkConst (n,ty);

fun destNullaryOp n tm =
    let
      val (n',ty) = Term.destConst tm
      val _ = Const.equal n n' orelse raise Error "Syntax.destNullaryOp"
    in
      ty
    end;

(* Unary operators *)

fun mkUnaryOp n (ty,a) =
    let
      val c = Term.mkConst (n,ty)
    in
      Term.mkApp (c,a)
    end;

fun destUnaryOp n tm =
    let
      val (c,a) = Term.destApp tm
      val (n',ty) = Term.destConst c
      val _ = Const.equal n n' orelse raise Error "Syntax.destUnaryOp"
    in
      (ty,a)
    end;

fun listMkUnaryOp n ty =
    let
      val c = Term.mkConst (n,ty)

      fun mk tm = Term.mkApp (c,tm)
    in
      fn (k,tm) => funpow k mk tm
    end;

fun stripUnaryOp n ty =
    let
      fun strip k tm =
          case total (destUnaryOp n) tm of
            NONE => (k,tm)
          | SOME (ty',tm') =>
            if Type.equal ty ty' then strip (k + 1) tm' else (k,tm)
    in
      strip 0
    end;

(* Binary operators *)

fun mkBinaryOp n (ty,a,b) =
    let
      val c = Term.mkConst (n,ty)
      val t = Term.mkApp (c,a)
    in
      Term.mkApp (t,b)
    end;

fun destBinaryOp n tm =
    let
      val (t,b) = Term.destApp tm
      val (c,a) = Term.destApp t
      val (n',ty) = Term.destConst c
      val _ = Const.equal n n' orelse raise Error "Syntax.destBinaryOp"
    in
      (ty,a,b)
    end;

fun listMkBinaryOp n ty tm tms =
    let
      val c = Term.mkConst (n,ty)

      fun mk (t,u) = Term.mkApp (Term.mkApp (c,t), u)
    in
      List.foldl mk tm tms
    end;

fun stripBinaryOp n ty =
    let
      fun strip l t =
          case total (destBinaryOp n) t of
            NONE => (t,l)
          | SOME (ty',a,t') =>
            if Type.equal ty ty' then strip (a :: l) t' else (t,l)
    in
      strip []
    end;

(* ------------------------------------------------------------------------- *)
(* Booleans.                                                                 *)
(* ------------------------------------------------------------------------- *)

(* True *)

val trueString = "T";

val trueName = Name.mkGlobal trueString;

fun trueConst sym = Symbol.mkConst sym trueName;

fun trueTerm sym = mkNullaryOp (trueConst sym) Type.bool;

fun isTrue sym = can (destNullaryOp (trueConst sym));

(* False *)

val falseString = "F";

val falseName = Name.mkGlobal falseString;

fun falseConst sym = Symbol.mkConst sym falseName;

fun falseTerm sym = mkNullaryOp (falseConst sym) Type.bool;

fun isFalse sym = can (destNullaryOp (falseConst sym));

(* ------------------------------------------------------------------------- *)
(* Propositional connectives.                                                *)
(* ------------------------------------------------------------------------- *)

(* Negations *)

val stringNeg = "~";

val nameNeg = Name.mkGlobal stringNeg;

fun constNeg sym = Symbol.mkConst sym nameNeg;

val tyNeg = Type.mkFun (Type.bool,Type.bool);

fun mkNeg sym =
    let
      val c = constNeg sym
    in
      fn tm => mkUnaryOp c (tyNeg,tm)
    end;

fun destNeg sym =
    let
      val c = constNeg sym
    in
      fn tm =>
         let
           val (_,a) = destUnaryOp c tm
         in
           a
         end
    end;

fun isNeg sym = can (destNeg sym);

fun listMkNeg sym =
    let
      val c = constNeg sym
    in
      listMkUnaryOp c tyNeg
    end;

fun stripNeg sym =
    let
      val c = constNeg sym
    in
      stripUnaryOp c tyNeg
    end;

(* Implications *)

val stringImp = "==>";

val nameImp = Name.mkGlobal stringImp;

val tyImp = Type.mkFun (Type.bool, Type.mkFun (Type.bool,Type.bool));

fun constImp sym = Symbol.mkConst sym nameImp;

fun mkImp sym =
    let
      val c = constImp sym
    in
      fn (a,b) => mkBinaryOp c (tyImp,a,b)
    end;

fun destImp sym =
    let
      val c = constImp sym
    in
      fn tm =>
         let
           val (_,a,b) = destBinaryOp c tm
         in
           (a,b)
         end
    end;

fun isImp sym = can (destImp sym);

fun listMkImp sym (tms,tm) =
    let
      val c = constImp sym
    in
      listMkBinaryOp c tyImp tm (rev tms)
    end;

fun stripImp sym tm =
    let
      val c = constImp sym
      val (tm,tms) = stripBinaryOp c tyImp tm
    in
      (rev tms, tm)
    end;

(* Conjunctions *)

val stringConj = "/\\";

val nameConj = Name.mkGlobal stringConj;

val tyConj = Type.mkFun (Type.bool, Type.mkFun (Type.bool,Type.bool));

fun constConj sym = Symbol.mkConst sym nameConj;

fun mkConj sym =
    let
      val c = constConj sym
    in
      fn (a,b) => mkBinaryOp c (tyConj,a,b)
    end;

fun destConj sym =
    let
      val c = constConj sym
    in
      fn tm =>
         let
           val (_,a,b) = destBinaryOp c tm
         in
           (a,b)
         end
    end;

fun isConj sym = can (destConj sym);

fun listMkConj sym tms =
    case rev tms of
      [] => trueTerm sym
    | tm :: tms =>
      let
        val c = constConj sym
      in
        listMkBinaryOp c tyConj tm tms
      end;

fun stripConj sym tm =
    if isTrue sym tm then []
    else
      let
        val c = constConj sym
        val (tm,tms) = stripBinaryOp c tyConj tm
      in
        rev (tm :: tms)
      end;

(* Disjunctions *)

val disjName = Name.mkGlobal "\\/";

val mkDisj =
    let
      val disjTy = mkFun (boolType, mkFun (boolType,boolType))
    in
      fn (a,b) => mkBinaryOp disjName (disjTy,a,b)
    end;

fun destDisj tm =
    let
      val (_,a,b) = destBinaryOp disjName tm
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
      mkApp (mkConst (forallName, forallType vTy), mkAbs (v,b))
    end;

fun destForall tm =
    let
      val (c,t) = destApp tm
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
      mkApp (mkConst (existsName, existsType vTy), mkAbs (v,b))
    end;

fun destExists tm =
    let
      val (c,t) = destApp tm
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
      mkApp (mkConst (existsUniqueName, existsUniqueType vTy), mkAbs (v,b))
    end;

fun destExistsUnique tm =
    let
      val (c,t) = destApp tm
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
      mkApp (mkConst (selectName, selectType vTy), mkAbs v_b)
    end;

fun destSelect tm =
    let
      val (_,t) = destUnaryOp selectName tm
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
