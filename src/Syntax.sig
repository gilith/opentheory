(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYNTAX                                                 *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Syntax =
sig

(* ------------------------------------------------------------------------- *)
(* Primitive.                                                                *)
(* ------------------------------------------------------------------------- *)

type name = Name.name
type ty = Type.ty
type var = Var.var
type term = Term.term
type sequent = Sequent.sequent
type thm = Thm.thm

(* Type variables *)

val mkTypeVar : name -> ty
val destTypeVar : ty -> name
val isTypeVar : ty -> bool
val equalTypeVar : name -> ty -> bool

val alphaTy : ty

(* Type operators *)

val mkTypeOp : name * ty list -> ty
val destTypeOp : ty -> name * ty list
val isTypeOp : ty -> bool

(* The type of booleans *)

val boolTy : ty

(* Function types *)

val mkFun : ty * ty -> ty
val destFun : ty -> ty * ty
val isFun : ty -> bool
val listMkFun : ty list * ty -> ty
val stripFun : ty -> ty list * ty

(* Constants *)

val mkConst : name * ty -> term
val destConst : term -> name * ty
val isConst : term -> bool

(* Variables *)

val mkVar : var -> term
val destVar : term -> var
val isVar : term -> bool
val equalVar : var -> term -> bool

(* Function applications *)

val mkComb : term * term -> term
val destComb : term -> term * term
val isComb : term -> bool
val rator : term -> term
val rand : term -> term
val land : term -> term
val listMkComb : term * term list -> term
val stripComb : term -> term * term list

(* Lambda abstractions *)

val mkAbs : var * term -> term
val destAbs : term -> var * term
val isAbs : term -> bool
val listMkAbs : var list * term -> term
val stripAbs : term -> var list * term

(* Equality *)

val eqTy : ty -> ty
val eqTm : term
val mkEq : term * term -> term
val destEq : term -> term * term
val isEq : term -> bool
val lhs : term -> term
val rhs : term -> term

(* Theorems *)

val axioms : thm -> SequentSet.set

val sequent : thm -> Sequent.sequent

val hyp : thm -> TermAlphaSet.set

val concl : thm -> term

(* ------------------------------------------------------------------------- *)
(* Operators.                                                                *)
(* ------------------------------------------------------------------------- *)

(* Unary operators *)

val mkUnop : name -> ty * term -> term
val destUnop : name -> term -> ty * term
val isUnop : name -> term -> bool

(* Binary operators *)

val mkBinop : name -> ty * term * term -> term
val destBinop : name -> term -> ty * term * term
val isBinop : name -> term -> bool

(* ------------------------------------------------------------------------- *)
(* Boolean.                                                                  *)
(* ------------------------------------------------------------------------- *)

(* True *)

val trueTm : term

(* False *)

val falseTm : term

(* Negations *)

val mkNeg : term -> term
val destNeg : term -> term
val isNeg : term -> bool

(* Implications *)

val mkImp : term * term -> term
val destImp : term -> term * term
val isImp : term -> bool

(* Universal quantifiers *)

val mkForall : var * term -> term
val destForall : term -> var * term
val isForall : term -> bool
val listMkForall : var list * term -> term
val stripForall : term -> var list * term

(* Existential quantifiers *)

val mkExists : var * term -> term
val destExists : term -> var * term
val isExists : term -> bool
val listMkExists : var list * term -> term
val stripExists : term -> var list * term

(* Unique existential quantifiers *)

val mkExistsUnique : var * term -> term
val destExistsUnique : term -> var * term
val isExistsUnique : term -> bool
val listMkExistsUnique : var list * term -> term
val stripExistsUnique : term -> var list * term

(* Hilbert's indefinite choice operator (epsilon) *)

val selectTy : ty -> ty
val selectTm : term
val mkSelect : var * term -> term
val destSelect : term -> var * term
val isSelect : term -> bool

(* ------------------------------------------------------------------------- *)
(* The type of individuals.                                                  *)
(* ------------------------------------------------------------------------- *)

val indTy : ty

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val showTypes : bool ref
val showHyp : bool ref

val ppType : ty Print.pp
val typeToString : ty -> string

val ppTerm : term Print.pp
val termToString : term -> string

val ppSubst : TermSubst.subst Print.pp
val substToString : TermSubst.subst -> string

val ppSequent : sequent Print.pp
val sequentToString : sequent -> string

val ppThm : thm Print.pp
val thmToString : thm -> string

end
