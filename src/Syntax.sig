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

(* Unary operators *)

val mkUnaryOp : Const.const -> Term.term * Type.ty -> Term.term

val destUnaryOp : Const.const -> Term.term -> Term.term * Type.ty

val listMkUnaryOp : Const.const -> Type.ty -> int * Term.term -> Term.term

val stripUnaryOp : Const.const -> Term.term -> int * Term.term

(* Binary operators *)

val mkBinaryOp : Const.const -> Term.term * Term.term * Type.ty -> Term.term

val destBinaryOp : Const.const -> Term.term -> Term.term * Term.term * Type.ty

val listMkBinaryOp :
    Const.const -> Type.ty -> Term.term * Term.term list -> Term.term

val stripBinaryOp : Const.const -> Term.term -> Term.term * Term.term list

(* Quantifiers *)

val mkQuant : Const.const -> Var.var * Term.term * Type.ty -> Term.term

val destQuant : Const.const -> Term.term -> Var.var * Term.term * Type.ty

val listMkQuant :
    Const.const -> Type.ty -> Var.var list * Term.term -> Term.term

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

val nameNeg : Name.name

val constNeg : Symbol.symbol -> Const.const

val tyNeg : Type.ty

val termNeg : Symbol.symbol -> Term.term

val mkNeg : Symbol.symbol -> Term.term -> Term.term

val destNeg : Symbol.symbol -> Term.term -> Term.term

val isNeg : Symbol.symbol -> Term.term -> bool

val listMkNeg : Symbol.symbol -> int * Term.term -> Term.term

val stripNeg : Symbol.symbol -> Term.term -> int * Term.term

(* Implications *)

val nameImp : Name.name

val constImp : Symbol.symbol -> Const.const

val tyImp : Type.ty

val termImp : Symbol.symbol -> Term.term

val mkImp : Symbol.symbol -> Term.term * Term.term -> Term.term

val destImp : Symbol.symbol -> Term.term -> Term.term * Term.term

val isImp : Symbol.symbol -> Term.term -> bool

val listMkImp : Symbol.symbol -> Term.term list * Term.term -> Term.term

val stripImp : Symbol.symbol -> Term.term -> Term.term list * Term.term

(* Conjunctions *)

val nameConj : Name.name

val constConj : Symbol.symbol -> Const.const

val tyConj : Type.ty

val termConj : Symbol.symbol -> Term.term

val mkConj : Symbol.symbol -> Term.term * Term.term -> Term.term

val destConj : Symbol.symbol -> Term.term -> Term.term * Term.term

val isConj : Symbol.symbol -> Term.term -> bool

val listMkConj : Symbol.symbol -> Term.term list -> Term.term

val stripConj : Symbol.symbol -> Term.term -> Term.term list

(* Disjunctions *)

val nameDisj : Name.name

val constDisj : Symbol.symbol -> Const.const

val tyDisj : Type.ty

val termDisj : Symbol.symbol -> Term.term

val mkDisj : Symbol.symbol -> Term.term * Term.term -> Term.term

val destDisj : Symbol.symbol -> Term.term -> Term.term * Term.term

val isDisj : Symbol.symbol -> Term.term -> bool

val listMkDisj : Symbol.symbol -> Term.term list -> Term.term

val stripDisj : Symbol.symbol -> Term.term -> Term.term list

(* ------------------------------------------------------------------------- *)
(* Quantifiers.                                                              *)
(* ------------------------------------------------------------------------- *)

(* Universal quantifiers *)

val nameForall : Name.name

val constForall : Symbol.symbol -> Const.const

val mkForall : Symbol.symbol -> Var.var * Term.term -> Term.term

val destForall : Symbol.symbol -> Term.term -> Var.var * Term.term

val isForall : Symbol.symbol -> Term.term -> bool

val listMkForall : Symbol.symbol -> Var.var list * Term.term -> Term.term

val stripForall : Symbol.symbol -> Term.term -> Var.var list * Term.term

(* Existential quantifiers *)

val nameExists : Name.name

val constExists : Symbol.symbol -> Const.const

val mkExists : Symbol.symbol -> Var.var * Term.term -> Term.term

val destExists : Symbol.symbol -> Term.term -> Var.var * Term.term

val isExists : Symbol.symbol -> Term.term -> bool

val listMkExists : Symbol.symbol -> Var.var list * Term.term -> Term.term

val stripExists : Symbol.symbol -> Term.term -> Var.var list * Term.term

(* Unique existential quantifiers *)

val nameExistsUnique : Name.name

val constExistsUnique : Symbol.symbol -> Const.const

val mkExistsUnique : Symbol.symbol -> Var.var * Term.term -> Term.term

val destExistsUnique : Symbol.symbol -> Term.term -> Var.var * Term.term

val isExistsUnique : Symbol.symbol -> Term.term -> bool

val listMkExistsUnique :
    Symbol.symbol -> Var.var list * Term.term -> Term.term

val stripExistsUnique :
    Symbol.symbol -> Term.term -> Var.var list * Term.term

(* Hilbert's indefinite choice operator (epsilon) *)

val nameSelect : Name.name

val constSelect : Symbol.symbol -> Const.const

val mkSelect : Symbol.symbol -> Var.var * Term.term -> Term.term

val destSelect : Symbol.symbol -> Term.term -> Var.var * Term.term

val isSelect : Symbol.symbol -> Term.term -> bool

(* ------------------------------------------------------------------------- *)
(* The type of individuals.                                                  *)
(* ------------------------------------------------------------------------- *)

val nameInd : Name.name

val typeOpInd : Symbol.symbol -> TypeOp.typeOp

val tyInd : Symbol.symbol -> Type.ty

end
