(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYNTAX                                                 *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Syntax :> Syntax =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Boolean syntax.                                                           *)
(* ------------------------------------------------------------------------- *)

(* Truth *)

fun constTrue sym = Symbol.mkConst sym Name.trueConst;

fun termTrue sym = Term.mkConst (constTrue sym, Type.bool);

(* Falsity *)

fun constFalse sym = Symbol.mkConst sym Name.falseConst;

fun termFalse sym = Term.mkConst (constFalse sym, Type.bool);

(* Negation *)

fun constNeg sym = Symbol.mkConst sym Name.negConst;

val typeNeg = Type.mkFun (Type.bool,Type.bool);

fun termNeg sym = Term.mkConst (constNeg sym, typeNeg);

fun mkNeg sym =
    let
      val c = termNeg sym
    in
      fn a => Term.mkApp (c,a)
    end;

(* Conjunction *)

fun constConj sym = Symbol.mkConst sym Name.conjConst;

val typeConj = Type.mkFun (Type.bool,typeNeg);

fun termConj sym = Term.mkConst (constConj sym, typeConj);

fun mkConj sym =
    let
      val c = termConj sym
    in
      fn (a,b) => Term.mkApp (Term.mkApp (c,a), b)
    end;

fun listMkConj sym tms =
    case rev tms of
      [] => termTrue sym
    | tm :: tms => List.foldl (mkConj sym) tm tms;

(* Disjunction *)

fun constDisj sym = Symbol.mkConst sym Name.disjConst;

val typeDisj = typeConj;

fun termDisj sym = Term.mkConst (constDisj sym, typeDisj);

fun mkDisj sym =
    let
      val c = termDisj sym
    in
      fn (a,b) => Term.mkApp (Term.mkApp (c,a), b)
    end;

fun listMkDisj sym tms =
    case rev tms of
      [] => termFalse sym
    | tm :: tms => List.foldl (mkDisj sym) tm tms;

(* Implication *)

fun constImp sym = Symbol.mkConst sym Name.impConst;

val typeImp = typeConj;

fun termImp sym = Term.mkConst (constImp sym, typeImp);

fun mkImp sym =
    let
      val c = termImp sym
    in
      fn (a,b) => Term.mkApp (Term.mkApp (c,a), b)
    end;

fun listMkImp sym (tms,tm) = List.foldl (mkImp sym) tm (List.rev tms);

(* Universal *)

fun constForall sym = Symbol.mkConst sym Name.forallConst;

fun mkTypeForall a = Type.mkFun (Type.mkFun (a,Type.bool), Type.bool);

fun mkTermForall sym =
    let
      val c = constForall sym
    in
      fn a => Term.mkConst (c, mkTypeForall a)
    end;

fun mkForall sym (v,b) =
    let
      val c = mkTermForall sym (Var.typeOf v)
    in
      Term.mkApp (c, Term.mkAbs (v,b))
    end;

fun listMkForall sym (vs,b) =
    List.foldl (mkForall sym) b (List.rev vs);

(* Existence *)

fun constExists sym = Symbol.mkConst sym Name.existsConst;

val mkTypeExists = mkTypeForall;

fun mkTermExists sym =
    let
      val c = constExists sym
    in
      fn a => Term.mkConst (c, mkTypeExists a)
    end;

fun mkExists sym (v,b) =
    let
      val c = mkTermExists sym (Var.typeOf v)
    in
      Term.mkApp (c, Term.mkAbs (v,b))
    end;

fun listMkExists sym (vs,b) =
    List.foldl (mkExists sym) b (List.rev vs);

(* Unique existence *)

fun constExistsUnique sym = Symbol.mkConst sym Name.existsUniqueConst;

val mkTypeExistsUnique = mkTypeForall;

fun mkTermExistsUnique sym =
    let
      val c = constExistsUnique sym
    in
      fn a => Term.mkConst (c, mkTypeExistsUnique a)
    end;

fun mkExistsUnique sym (v,b) =
    let
      val c = mkTermExistsUnique sym (Var.typeOf v)
    in
      Term.mkApp (c, Term.mkAbs (v,b))
    end;

fun listMkExistsUnique sym (vs,b) =
    List.foldl (mkExistsUnique sym) b (List.rev vs);

end
