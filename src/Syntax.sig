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

val constTrue : Symbol.symbol -> Const.const

val termTrue : Symbol.symbol -> Term.term

(* Falsity *)

val constFalse : Symbol.symbol -> Const.const

val termFalse : Symbol.symbol -> Term.term

(* Negation *)

val constNeg : Symbol.symbol -> Const.const

val typeNeg : Type.ty

val termNeg : Symbol.symbol -> Term.term

val mkNeg : Symbol.symbol -> Term.term -> Term.term

(* Conjunction *)

val constConj : Symbol.symbol -> Const.const

val typeConj : Type.ty

val termConj : Symbol.symbol -> Term.term

val mkConj : Symbol.symbol -> Term.term * Term.term -> Term.term

val listMkConj : Symbol.symbol -> Term.term list -> Term.term

(* Disjunction *)

val constDisj : Symbol.symbol -> Const.const

val typeDisj : Type.ty

val termDisj : Symbol.symbol -> Term.term

val mkDisj : Symbol.symbol -> Term.term * Term.term -> Term.term

val listMkDisj : Symbol.symbol -> Term.term list -> Term.term

(* Implication *)

val constImp : Symbol.symbol -> Const.const

val typeImp : Type.ty

val termImp : Symbol.symbol -> Term.term

val mkImp : Symbol.symbol -> Term.term * Term.term -> Term.term

val listMkImp : Symbol.symbol -> Term.term list * Term.term -> Term.term

(* Universal *)

val constForall : Symbol.symbol -> Const.const

val mkTypeForall : Type.ty -> Type.ty

val mkTermForall : Symbol.symbol -> Type.ty -> Term.term

val mkForall : Symbol.symbol -> Var.var * Term.term -> Term.term

val listMkForall : Symbol.symbol -> Var.var list * Term.term -> Term.term

(* Existence *)

val constExists : Symbol.symbol -> Const.const

val mkTypeExists : Type.ty -> Type.ty

val mkTermExists : Symbol.symbol -> Type.ty -> Term.term

val mkExists : Symbol.symbol -> Var.var * Term.term -> Term.term

val listMkExists : Symbol.symbol -> Var.var list * Term.term -> Term.term

(* Unique existence *)

val constExistsUnique : Symbol.symbol -> Const.const

val mkTypeExistsUnique : Type.ty -> Type.ty

val mkTermExistsUnique : Symbol.symbol -> Type.ty -> Term.term

val mkExistsUnique : Symbol.symbol -> Var.var * Term.term -> Term.term

val listMkExistsUnique : Symbol.symbol -> Var.var list * Term.term -> Term.term

end
