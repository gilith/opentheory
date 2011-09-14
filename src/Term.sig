(* ========================================================================= *)
(* HIGHER ORDER LOGIC TERMS                                                  *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Term =
sig

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic terms.                                       *)
(* ------------------------------------------------------------------------- *)

type term = TypeTerm.term

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

type term' = TypeTerm.term'

val mk : term' -> term

val dest : term -> term'

(* Constants *)

val mkConst : Const.const * Type.ty -> term

val destConst : term -> Const.const * Type.ty

val isConst : term -> bool

(* Variables *)

val mkVar : Var.var -> term

val destVar : term -> Var.var

val isVar : term -> bool

val equalVar : Var.var -> term -> bool

(* Function applications *)

val mkApp : term * term -> term

val destApp : term -> term * term

val isApp : term -> bool

val rator : term -> term

val rand : term -> term

val land : term -> term

val listMkApp : term * term list -> term

val stripApp : term -> term * term list

(* Lambda abstractions *)

val mkAbs : Var.var * term -> term

val destAbs : term -> Var.var * term

val isAbs : term -> bool

val listMkAbs : Var.var list * term -> term

val stripAbs : term -> Var.var list * term

(* ------------------------------------------------------------------------- *)
(* Term IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

type id = TypeTerm.id

val id : term -> id

val equalId : id -> term -> bool

(* ------------------------------------------------------------------------- *)
(* Number of constructors.                                                   *)
(* ------------------------------------------------------------------------- *)

val size : term -> int

val sizeList : term list -> int

(* ------------------------------------------------------------------------- *)
(* A total order on terms, with and without alpha equivalence.               *)
(* ------------------------------------------------------------------------- *)

val compare : term * term -> order

val equal : term -> term -> bool

val checkEqual : term -> term -> unit

val alphaCompare : term * term -> order

val alphaEqual : term -> term -> bool

val checkAlphaEqual : term -> term -> unit

(* ------------------------------------------------------------------------- *)
(* The type of a term.                                                       *)
(* ------------------------------------------------------------------------- *)

val typeOf : term -> Type.ty

(* ------------------------------------------------------------------------- *)
(* Checking that a term has type bool.                                       *)
(* ------------------------------------------------------------------------- *)

val isBool : term -> bool

val checkBool : term -> unit

(* ------------------------------------------------------------------------- *)
(* Free variables.                                                           *)
(* ------------------------------------------------------------------------- *)

type sharingFreeVars

val newSharingFreeVars : sharingFreeVars

val sharingFreeVars : term -> sharingFreeVars -> VarSet.set * sharingFreeVars

val freeVarsList : term list -> VarSet.set

val freeVars : term -> VarSet.set

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

type sharingConsts

val emptySharingConsts : sharingConsts

val addConstSharingConsts : Const.const -> sharingConsts -> sharingConsts

val addConstSetSharingConsts : ConstSet.set -> sharingConsts -> sharingConsts

val addSharingConsts : term -> sharingConsts -> sharingConsts

val addListSharingConsts : term list -> sharingConsts -> sharingConsts

val unionSharingConsts : sharingConsts -> sharingConsts -> sharingConsts

val toSetSharingConsts : sharingConsts -> ConstSet.set

val consts : term -> ConstSet.set

val constsList : term list -> ConstSet.set

(* ------------------------------------------------------------------------- *)
(* Type variables.                                                           *)
(* ------------------------------------------------------------------------- *)

type sharingTypeVars

val emptySharingTypeVars : sharingTypeVars

val addSharingTypeVars : term -> sharingTypeVars -> sharingTypeVars

val addListSharingTypeVars : term list -> sharingTypeVars -> sharingTypeVars

val toSetSharingTypeVars : sharingTypeVars -> NameSet.set

val typeVars : term -> NameSet.set

val typeVarsList : term list -> NameSet.set

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

type sharingTypeOps

val emptySharingTypeOps : sharingTypeOps

val addTypeOpSharingTypeOps : TypeOp.typeOp -> sharingTypeOps -> sharingTypeOps

val addTypeOpSetSharingTypeOps :
    TypeOpSet.set -> sharingTypeOps -> sharingTypeOps

val addTypeSharingTypeOps : Type.ty -> sharingTypeOps -> sharingTypeOps

val addSharingTypeOps : term -> sharingTypeOps -> sharingTypeOps

val addListSharingTypeOps : term list -> sharingTypeOps -> sharingTypeOps

val unionSharingTypeOps : sharingTypeOps -> sharingTypeOps -> sharingTypeOps

val toSetSharingTypeOps : sharingTypeOps -> TypeOpSet.set

val typeOps : term -> TypeOpSet.set

val typeOpsList : term list -> TypeOpSet.set

(* ------------------------------------------------------------------------- *)
(* Primitive constants.                                                      *)
(* ------------------------------------------------------------------------- *)

(* Equality *)

val mkEqConst : Type.ty -> term

val destEqConst : term -> Type.ty

val isEqConst : term -> bool

val mkEq : term * term -> term

val destEq : term -> term * term

val isEq : term -> bool

val lhs : term -> term

val rhs : term -> term

val mkRefl : term -> term

val destRefl : term -> term

val isRefl : term -> bool

(* Hilbert's choice operator *)

val mkSelectConst : Type.ty -> term

val destSelectConst : term -> Type.ty

val isSelectConst : term -> bool

val mkSelect : Var.var * term -> term

val destSelect : term -> Var.var * term

val isSelect : term -> bool

(* ------------------------------------------------------------------------- *)
(* Axioms.                                                                   *)
(* ------------------------------------------------------------------------- *)

val axiomOfExtensionality : term

val axiomOfChoice : term

val axiomOfInfinity : term

(* ------------------------------------------------------------------------- *)
(* General syntax operations.                                                *)
(* ------------------------------------------------------------------------- *)

(* Nullary operators *)

val isNullaryOp : (Const.const -> bool) -> term -> bool

(* Unary operators *)

val destUnaryOp : (Const.const -> bool) -> term -> term

(* Binary operators *)

val destBinaryOp : (Const.const -> bool) -> term -> term * term

val stripBinaryOp : (Const.const -> bool) -> term -> term list * term

(* Ternary operators *)

val destTernaryOp : (Const.const -> bool) -> term -> term * term * term

(* Quantifiers *)

val destQuant : (Const.const -> bool) -> term -> Var.var * term

val stripQuant : (Const.const -> bool) -> term -> Var.var list * term

(* ------------------------------------------------------------------------- *)
(* Special syntax.                                                           *)
(* ------------------------------------------------------------------------- *)

(* Truth *)

val isTrue : term -> bool

(* Falsity *)

val isFalse : term -> bool

(* Negation *)

val isNegConst : term -> bool

val destNeg : term -> term

val isNeg : term -> bool

(* Conjunction *)

val isConjConst : term -> bool

val destConj : term -> term * term

val isConj : term -> bool

val stripConj : term -> term list

(* Disjunction *)

val isDisjConst : term -> bool

val destDisj : term -> term * term

val isDisj : term -> bool

val stripDisj : term -> term list

(* Implication *)

val isImpConst : term -> bool

val destImp : term -> term * term

val isImp : term -> bool

val stripImp : term -> term list * term

(* Universal *)

val isForallConst : term -> bool

val destForall : term -> Var.var * term

val isForall : term -> bool

val stripForall : term -> Var.var list * term

(* Existence *)

val isExistsConst : term -> bool

val destExists : term -> Var.var * term

val isExists : term -> bool

val stripExists : term -> Var.var list * term

(* Unique existence *)

val isExistsUniqueConst : term -> bool

val destExistsUnique : term -> Var.var * term

val isExistsUnique : term -> bool

val stripExistsUnique : term -> Var.var list * term

(* Conditional *)

val isCondConst : term -> bool

val destCond : term -> term * term * term

val isCond : term -> bool

(* Generalized abstractions *)

val destGenAbs : term -> term * term

val isGenAbs : term -> bool

val stripGenAbs : term -> term list * term

(* Let bindings *)

val destLet : term -> term * term * term

val isLet : term -> bool

(* Numerals *)

val destNumeral : term -> int

val isNumeral : term -> bool

(* Set comprehensions *)

val destComprehension : term -> Var.var * Var.var list * term * term

val isComprehension : term -> bool

(* Case expressions *)

val destCase : term -> term * (Name.name * term list * term) list

val isCase : term -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype grammar =
    Grammar of
      {negations : Print.token list,
       infixes : Print.infixes,
       binders : Print.token list,
       showConst : Show.show -> Const.const * Type.ty -> Name.name,
       ppSyntax : string Print.pp,
       ppVar : Show.show -> Var.var Print.pp,
       ppConst : Show.show -> (Const.const * Type.ty) Print.pp,
       ppNegation : Show.show -> (Const.const * Type.ty) Print.pp,
       ppInfix : Show.show -> (Const.const * Type.ty) Print.pp,
       ppBinder : Show.show -> (Const.const * Type.ty) option Print.pp,
       ppNumeral : Show.show -> (int * Type.ty) Print.pp,
       maximumSize : int}

val defaultGrammar : grammar

val ppWithGrammar : grammar -> Show.show -> term Print.pp

val ppWithShow : Show.show -> term Print.pp

val pp : term Print.pp

val toString : term -> string

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

val htmlGrammar : grammar

val ppHtml : Show.show -> term Print.pp

end
