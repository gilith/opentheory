(* ========================================================================= *)
(* OPENTHEORY OBJECTS                                                        *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Object =
sig

(* ------------------------------------------------------------------------- *)
(* A type of OpenTheory objects.                                             *)
(* ------------------------------------------------------------------------- *)

datatype object =
    Num of int
  | Name of Name.name
  | TypeOp of TypeOp.typeOp
  | Type of Type.ty
  | Const of Const.const
  | Var of Var.var
  | Term of Term.term
  | Thm of Thm.thm
  | List of object list

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

(* List objects *)

val destList : object -> object list

val isList : object -> bool

(* Num objects *)

val destNum : object -> int

val isNum : object -> bool

(* Name objects *)

val destName : object -> Name.name

val isName : object -> bool

(* Name list objects *)

val mkNames : Name.name list -> object

val destNames : object -> Name.name list

val isNames : object -> bool

(* Unit objects *)

val unit : object

val mkUnit : unit -> object

val isUnit : object -> bool

(* Pair objects *)

val mkPair : object * object -> object

val destPair : object -> object * object

val isPair : object -> bool

(* Triple objects *)

val mkTriple : object * object * object -> object

val destTriple : object -> object * object * object

val isTriple : object -> bool

(* Type operator objects *)

val destTypeOp : object -> TypeOp.typeOp

val isTypeOp : object -> bool

(* Type objects *)

val destType : object -> Type.ty

val isType : object -> bool

(* Type list objects *)

val mkTypes : Type.ty list -> object

val destTypes : object -> Type.ty list

val isTypes : object -> bool

(* Type variable type objects *)

val mkVarType : Name.name -> object

val destVarType : object -> Name.name

val isVarType : object -> bool

(* Type operator type objects *)

val mkOpType : TypeOp.typeOp * Type.ty list -> object

val destOpType : object -> TypeOp.typeOp * Type.ty list

val isOpType : object -> bool

(* Constant objects *)

val destConst : object -> Const.const

val isConst : object -> bool

(* Term variable objects *)

val destVar : object -> Var.var

val isVar : object -> bool

(* Term objects *)

val destTerm : object -> Term.term

val isTerm : object -> bool

(* Term list objects *)

val mkTerms : Term.term list -> object

val destTerms : object -> Term.term list

val isTerms : object -> bool

(* Term variable term objects *)

val mkVarTerm : Var.var -> object

val destVarTerm : object -> Var.var

val isVarTerm : object -> bool

(* Constant term objects *)

val mkConstTerm : Const.const * Type.ty -> object

val destConstTerm : object -> Const.const * Type.ty

val isConstTerm : object -> bool

(* Function application term objects *)

val mkAppTerm : Term.term * Term.term -> object

val destAppTerm : object -> Term.term * Term.term

val isAppTerm : object -> bool

(* Lambda abstraction term objects *)

val mkAbsTerm : Var.var * Term.term -> object

val destAbsTerm : object -> Var.var * Term.term

val isAbsTerm : object -> bool

(* Sequent objects *)

val mkSequent : Sequent.sequent -> object * object

val destSequent : object * object -> Sequent.sequent

val isSequent : object * object -> bool

(* Theorem objects *)

val destThm : object -> Thm.thm

val isThm : object -> bool

(* Type substitution objects *)

val mkTypeSubst : TypeSubst.substMap -> object

val destTypeSubst : object -> TypeSubst.substMap

val isTypeSubst : object -> bool

(* Term substitution objects *)

val mkTermSubst : TermSubst.termSubstMap -> object

val destTermSubst : object -> TermSubst.termSubstMap

val isTermSubst : object -> bool

(* Substitution objects *)

val mkSubst : TermSubst.substMap -> object

val destSubst : object -> TermSubst.substMap

val isSubst : object -> bool

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

val compare : object * object -> order

(* ------------------------------------------------------------------------- *)
(* Extracting the theorems stored in an object.                              *)
(* ------------------------------------------------------------------------- *)

val thms : object -> Thm.thm list

(* ------------------------------------------------------------------------- *)
(* Extracting the symbols in an object.                                      *)
(* ------------------------------------------------------------------------- *)

val symbol : object -> Symbol.symbol

val symbolAdd : Symbol.symbol -> object -> Symbol.symbol

val symbolAddList : Symbol.symbol -> object list -> Symbol.symbol

(* ------------------------------------------------------------------------- *)
(* Breaking down objects into commands.                                      *)
(* ------------------------------------------------------------------------- *)

val command : object -> Command.command * object list

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : object Print.pp

end
