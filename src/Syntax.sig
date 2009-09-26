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

val constTrue : Symbol.symbol list -> Const.const

val termTrue : Symbol.symbol list -> Term.term

val isTrue : Symbol.symbol list -> Term.term -> bool

(* False *)

val nameFalse : Name.name

val constFalse : Symbol.symbol list -> Const.const

val termFalse : Symbol.symbol list -> Term.term

val isFalse : Symbol.symbol list -> Term.term -> bool

(* ------------------------------------------------------------------------- *)
(* Propositional connectives.                                                *)
(* ------------------------------------------------------------------------- *)

(* Negations *)

val nameNeg : Name.name

val constNeg : Symbol.symbol list -> Const.const

val tyNeg : Type.ty

val termNeg : Symbol.symbol list -> Term.term

val mkNeg : Symbol.symbol list -> Term.term -> Term.term

val destNeg : Symbol.symbol list -> Term.term -> Term.term

val isNeg : Symbol.symbol list -> Term.term -> bool

val listMkNeg : Symbol.symbol list -> int * Term.term -> Term.term

val stripNeg : Symbol.symbol list -> Term.term -> int * Term.term

(* Implications *)

val nameImp : Name.name

val constImp : Symbol.symbol list -> Const.const

val tyImp : Type.ty

val termImp : Symbol.symbol list -> Term.term

val mkImp : Symbol.symbol list -> Term.term * Term.term -> Term.term

val destImp : Symbol.symbol list -> Term.term -> Term.term * Term.term

val isImp : Symbol.symbol list -> Term.term -> bool

val listMkImp : Symbol.symbol list -> Term.term list * Term.term -> Term.term

val stripImp : Symbol.symbol list -> Term.term -> Term.term list * Term.term

(* Conjunctions *)

val nameConj : Name.name

val constConj : Symbol.symbol list -> Const.const

val tyConj : Type.ty

val termConj : Symbol.symbol list -> Term.term

val mkConj : Symbol.symbol list -> Term.term * Term.term -> Term.term

val destConj : Symbol.symbol list -> Term.term -> Term.term * Term.term

val isConj : Symbol.symbol list -> Term.term -> bool

val listMkConj : Symbol.symbol list -> Term.term list -> Term.term

val stripConj : Symbol.symbol list -> Term.term -> Term.term list

(* Disjunctions *)

val nameDisj : Name.name

val constDisj : Symbol.symbol list -> Const.const

val tyDisj : Type.ty

val termDisj : Symbol.symbol list -> Term.term

val mkDisj : Symbol.symbol list -> Term.term * Term.term -> Term.term

val destDisj : Symbol.symbol list -> Term.term -> Term.term * Term.term

val isDisj : Symbol.symbol list -> Term.term -> bool

val listMkDisj : Symbol.symbol list -> Term.term list -> Term.term

val stripDisj : Symbol.symbol list -> Term.term -> Term.term list

(* ------------------------------------------------------------------------- *)
(* Quantifiers.                                                              *)
(* ------------------------------------------------------------------------- *)

(* Universal quantifiers *)

val nameForall : Name.name

val constForall : Symbol.symbol list -> Const.const

val mkForall : Symbol.symbol list -> Var.var * Term.term -> Term.term

val destForall : Symbol.symbol list -> Term.term -> Var.var * Term.term

val isForall : Symbol.symbol list -> Term.term -> bool

val listMkForall : Symbol.symbol list -> Var.var list * Term.term -> Term.term

val stripForall : Symbol.symbol list -> Term.term -> Var.var list * Term.term

(* Existential quantifiers *)

val nameExists : Name.name

val constExists : Symbol.symbol list -> Const.const

val mkExists : Symbol.symbol list -> Var.var * Term.term -> Term.term

val destExists : Symbol.symbol list -> Term.term -> Var.var * Term.term

val isExists : Symbol.symbol list -> Term.term -> bool

val listMkExists : Symbol.symbol list -> Var.var list * Term.term -> Term.term

val stripExists : Symbol.symbol list -> Term.term -> Var.var list * Term.term

(* Unique existential quantifiers *)

val nameExistsUnique : Name.name

val constExistsUnique : Symbol.symbol list -> Const.const

val mkExistsUnique : Symbol.symbol list -> Var.var * Term.term -> Term.term

val destExistsUnique : Symbol.symbol list -> Term.term -> Var.var * Term.term

val isExistsUnique : Symbol.symbol list -> Term.term -> bool

val listMkExistsUnique :
    Symbol.symbol list -> Var.var list * Term.term -> Term.term

val stripExistsUnique :
    Symbol.symbol list -> Term.term -> Var.var list * Term.term

(* Hilbert's indefinite choice operator (epsilon) *)

val nameSelect : Name.name

val constSelect : Symbol.symbol list -> Const.const

val mkSelect : Symbol.symbol list -> Var.var * Term.term -> Term.term

val destSelect : Symbol.symbol list -> Term.term -> Var.var * Term.term

val isSelect : Symbol.symbol list -> Term.term -> bool

(* ------------------------------------------------------------------------- *)
(* The type of individuals.                                                  *)
(* ------------------------------------------------------------------------- *)

val nameInd : Name.name

val typeOpInd : Symbol.symbol list -> TypeOp.typeOp

val tyInd : Symbol.symbol list -> Type.ty

end
