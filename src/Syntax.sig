(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYNTAX                                                 *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Syntax =
sig

(* ------------------------------------------------------------------------- *)
(* Boolean syntax.                                                           *)
(* ------------------------------------------------------------------------- *)

(* Truth *)

val constTrue : Symbol.table -> Const.const

val termTrue : Symbol.table -> Term.term

(* Falsity *)

val constFalse : Symbol.table -> Const.const

val termFalse : Symbol.table -> Term.term

(* Negation *)

val constNeg : Symbol.table -> Const.const

val typeNeg : Type.ty

val termNeg : Symbol.table -> Term.term

val mkNeg : Symbol.table -> Term.term -> Term.term

(* Conjunction *)

val constConj : Symbol.table -> Const.const

val typeConj : Type.ty

val termConj : Symbol.table -> Term.term

val mkConj : Symbol.table -> Term.term * Term.term -> Term.term

val listMkConj : Symbol.table -> Term.term list -> Term.term

(* Disjunction *)

val constDisj : Symbol.table -> Const.const

val typeDisj : Type.ty

val termDisj : Symbol.table -> Term.term

val mkDisj : Symbol.table -> Term.term * Term.term -> Term.term

val listMkDisj : Symbol.table -> Term.term list -> Term.term

(* Implication *)

val constImp : Symbol.table -> Const.const

val typeImp : Type.ty

val termImp : Symbol.table -> Term.term

val mkImp : Symbol.table -> Term.term * Term.term -> Term.term

val listMkImp : Symbol.table -> Term.term list * Term.term -> Term.term

(* Universal *)

val constForall : Symbol.table -> Const.const

val mkTypeForall : Type.ty -> Type.ty

val mkTermForall : Symbol.table -> Type.ty -> Term.term

val mkForall : Symbol.table -> Var.var * Term.term -> Term.term

val listMkForall : Symbol.table -> Var.var list * Term.term -> Term.term

(* Existence *)

val constExists : Symbol.table -> Const.const

val mkTypeExists : Type.ty -> Type.ty

val mkTermExists : Symbol.table -> Type.ty -> Term.term

val mkExists : Symbol.table -> Var.var * Term.term -> Term.term

val listMkExists : Symbol.table -> Var.var list * Term.term -> Term.term

(* Unique existence *)

val constExistsUnique : Symbol.table -> Const.const

val mkTypeExistsUnique : Type.ty -> Type.ty

val mkTermExistsUnique : Symbol.table -> Type.ty -> Term.term

val mkExistsUnique : Symbol.table -> Var.var * Term.term -> Term.term

val listMkExistsUnique : Symbol.table -> Var.var list * Term.term -> Term.term

end
