(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYNTAX                                                 *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Syntax =
sig

(* ------------------------------------------------------------------------- *)
(* Operators.                                                                *)
(* ------------------------------------------------------------------------- *)

(* Nullary operators *)

val mkNullaryOp : Const.const -> Type.ty -> Term.term

val destNullaryOp : Const.const -> Term.term -> Type.ty

val isNullaryOp : Const.const -> Term.term -> bool

(* Unary operators *)

val mkUnaryOp : Const.const -> Type.ty * Term.term -> Term.term

val destUnaryOp : Const.const -> Term.term -> Type.ty * Term.term

val isUnaryOp : Const.const -> Term.term -> bool

val listMkUnaryOp : Const.const -> Type.ty -> int * Term.term -> Term.term

val stripUnaryOp : Const.const -> Type.ty -> Term.term -> int * Term.term

(* Binary operators *)

val mkBinaryOp : Const.const -> Type.ty * Term.term * Term.term -> Term.term

val destBinaryOp : Const.const -> Term.term -> Type.ty * Term.term * Term.term

val isBinaryOp : Const.const -> Term.term -> bool

val listMkBinaryOp :
    Const.const -> Type.ty -> Term.term -> Term.term list -> Term.term

val stripBinaryOp :
    Const.const -> Type.ty -> Term.term -> Term.term -> Term.term list

(* ------------------------------------------------------------------------- *)
(* Quantifier operators.                                                     *)
(* ------------------------------------------------------------------------- *)

val mkQuant : Const.const -> Var.var * Term.term -> Term.term

val destQuant : Const.const -> Term.term -> Var.var * Term.term

val isQuant : Const.const -> Term.term -> bool

val listMkQuant : Const.const -> Var.var list * Term.term -> Term.term

val stripQuant : Const.const -> Term.term -> Var.var list * Term.term

(* ------------------------------------------------------------------------- *)
(* Booleans.                                                                 *)
(* ------------------------------------------------------------------------- *)

(* True *)

val nameTrue : Name.name

val constTrue : Symbol.symbol -> Const.const

val termTrue : Symbol.symbol -> Term.term

val isTrue : Symbol.symbol -> Term.term -> bool

(* False *)

val nameFalse : Name.name

val constFalse : Symbol.symbol -> Const.const

val termFalse : Symbol.symbol -> Term.term

val isFalse : Symbol.symbol -> Term.term -> bool

(* ------------------------------------------------------------------------- *)
(* Propositional connectives.                                                *)
(* ------------------------------------------------------------------------- *)

(* Negations *)

val tyNeg : Type.ty

val mkNeg : Symbol.symbol -> Term.term -> Term.term

val destNeg : Symbol.symbol -> Term.term -> Term.term

val isNeg : Symbol.symbol -> Term.term -> bool

val listMkNeg : Symbol.symbol -> int * Term.term -> Term.term

val stripNeg : Symbol.symbol -> Term.term -> int * Term.term

(* Implications *)

val tyImp : Type.ty

val mkImp : Symbol.symbol -> Term.term * Term.term -> Term.term

val destImp : Symbol.symbol -> Term.term -> Term.term * Term.term

val isImp : Symbol.symbol -> Term.term -> bool

val listMkImp : Symbol.symbol -> Term.term list * Term.term -> Term.term

val stripImp : Symbol.symbol -> Term.term -> Term.term list * Term.term

(* Conjunctions *)

val mkConj : Term.term * Term.term -> Term.term

val destConj : Term.term -> Term.term * Term.term

val isConj : Term.term -> bool

val listMkConj : Term.term list -> Term.term

val stripConj : Term.term -> Term.term list

(* Disjunctions *)

val mkDisj : Term.term * Term.term -> Term.term

val destDisj : Term.term -> Term.term * Term.term

val isDisj : Term.term -> bool

val listMkDisj : Term.term list -> Term.term

val stripDisj : Term.term -> Term.term list

(* ------------------------------------------------------------------------- *)
(* Quantifiers.                                                              *)
(* ------------------------------------------------------------------------- *)

(* Universal quantifiers *)

val mkForall : Var.var * Term.term -> Term.term

val destForall : Term.term -> Var.var * Term.term

val isForall : Term.term -> bool

val listMkForall : Var.var list * Term.term -> Term.term

val stripForall : Term.term -> Var.var list * Term.term

(* Existential quantifiers *)

val mkExists : Var.var * Term.term -> Term.term

val destExists : Term.term -> Var.var * Term.term

val isExists : Term.term -> bool

val listMkExists : Var.var list * Term.term -> Term.term

val stripExists : Term.term -> Var.var list * Term.term

(* Unique existential quantifiers *)

val mkExistsUnique : Var.var * Term.term -> Term.term

val destExistsUnique : Term.term -> Var.var * Term.term

val isExistsUnique : Term.term -> bool

val listMkExistsUnique : Var.var list * Term.term -> Term.term

val stripExistsUnique : Term.term -> Var.var list * Term.term

(* Hilbert's indefinite choice operator (epsilon) *)

val selectType : Type.ty -> Type.ty

val selectTerm : Term.term

val mkSelect : Var.var * Term.term -> Term.term

val destSelect : Term.term -> Var.var * Term.term

val isSelect : Term.term -> bool

(* ------------------------------------------------------------------------- *)
(* The type of individuals.                                                  *)
(* ------------------------------------------------------------------------- *)

val typeOpInd : Symbol.symbol -> TypeOp.typeOp

val typeInd : Symbol.symbol -> Type.ty

end
