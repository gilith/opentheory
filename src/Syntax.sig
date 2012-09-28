(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYNTAX                                                 *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature Syntax =
sig

(* ------------------------------------------------------------------------- *)
(* Boolean syntax.                                                           *)
(* ------------------------------------------------------------------------- *)

(* Truth *)

val constTrue : SymbolTable.table -> Const.const

val termTrue : SymbolTable.table -> Term.term

(* Falsity *)

val constFalse : SymbolTable.table -> Const.const

val termFalse : SymbolTable.table -> Term.term

(* Negation *)

val constNeg : SymbolTable.table -> Const.const

val typeNeg : Type.ty

val termNeg : SymbolTable.table -> Term.term

val mkNeg : SymbolTable.table -> Term.term -> Term.term

(* Conjunction *)

val constConj : SymbolTable.table -> Const.const

val typeConj : Type.ty

val termConj : SymbolTable.table -> Term.term

val mkConj : SymbolTable.table -> Term.term * Term.term -> Term.term

val listMkConj : SymbolTable.table -> Term.term list -> Term.term

(* Disjunction *)

val constDisj : SymbolTable.table -> Const.const

val typeDisj : Type.ty

val termDisj : SymbolTable.table -> Term.term

val mkDisj : SymbolTable.table -> Term.term * Term.term -> Term.term

val listMkDisj : SymbolTable.table -> Term.term list -> Term.term

(* Implication *)

val constImp : SymbolTable.table -> Const.const

val typeImp : Type.ty

val termImp : SymbolTable.table -> Term.term

val mkImp : SymbolTable.table -> Term.term * Term.term -> Term.term

val listMkImp : SymbolTable.table -> Term.term list * Term.term -> Term.term

(* Universal *)

val constForall : SymbolTable.table -> Const.const

val mkTypeForall : Type.ty -> Type.ty

val mkTermForall : SymbolTable.table -> Type.ty -> Term.term

val mkForall : SymbolTable.table -> Var.var * Term.term -> Term.term

val listMkForall : SymbolTable.table -> Var.var list * Term.term -> Term.term

(* Existence *)

val constExists : SymbolTable.table -> Const.const

val mkTypeExists : Type.ty -> Type.ty

val mkTermExists : SymbolTable.table -> Type.ty -> Term.term

val mkExists : SymbolTable.table -> Var.var * Term.term -> Term.term

val listMkExists : SymbolTable.table -> Var.var list * Term.term -> Term.term

(* Unique existence *)

val constExistsUnique : SymbolTable.table -> Const.const

val mkTypeExistsUnique : Type.ty -> Type.ty

val mkTermExistsUnique : SymbolTable.table -> Type.ty -> Term.term

val mkExistsUnique : SymbolTable.table -> Var.var * Term.term -> Term.term

val listMkExistsUnique :
    SymbolTable.table -> Var.var list * Term.term -> Term.term

(* ------------------------------------------------------------------------- *)
(* Pair syntax.                                                              *)
(* ------------------------------------------------------------------------- *)

(* Pair type operator *)

val typeOpPair : SymbolTable.table -> TypeOp.typeOp

val mkTypePair : SymbolTable.table -> Type.ty * Type.ty -> Type.ty

(* Pair constant *)

val constPair : SymbolTable.table -> Const.const

val mkTermPair : SymbolTable.table -> Type.ty * Type.ty -> Term.term

val mkPair : SymbolTable.table -> Term.term * Term.term -> Term.term

val listMkPair : SymbolTable.table -> Term.term list -> Term.term

end
