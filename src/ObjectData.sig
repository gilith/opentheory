(* ========================================================================= *)
(* OPENTHEORY OBJECT DATA                                                    *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature ObjectData =
sig

(* ------------------------------------------------------------------------- *)
(* A type of OpenTheory object data.                                         *)
(* ------------------------------------------------------------------------- *)

datatype data =
    Num of int
  | Name of Name.name
  | TypeOp of TypeOp.typeOp
  | Type of Type.ty
  | Const of Const.const
  | Var of Var.var
  | Term of Term.term
  | Thm of Thm.thm
  | List of data list

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

(* List data *)

val destList : data -> data list

val isList : data -> bool

val mkNil : data

val isNil : data -> bool

val mkCons : data * data -> data

val destCons : data -> data * data

val isCons : data -> bool

(* Num data *)

val destNum : data -> int

val isNum : data -> bool

(* Name data *)

val destName : data -> Name.name

val isName : data -> bool

(* Name list data *)

val mkNames : Name.name list -> data

val destNames : data -> Name.name list

val isNames : data -> bool

(* Unit data *)

val unit : data

val mkUnit : unit -> data

val isUnit : data -> bool

(* Pair data *)

val mkPair : data * data -> data

val destPair : data -> data * data

val isPair : data -> bool

(* Triple data *)

val mkTriple : data * data * data -> data

val destTriple : data -> data * data * data

val isTriple : data -> bool

(* Type operator data *)

val destTypeOp : data -> TypeOp.typeOp

val isTypeOp : data -> bool

val equalTypeOp : TypeOp.typeOp -> data -> bool

(* Type data *)

val destType : data -> Type.ty

val isType : data -> bool

(* Type list data *)

val mkTypes : Type.ty list -> data

val destTypes : data -> Type.ty list

val isTypes : data -> bool

(* Type variable type data *)

val mkVarType : Name.name -> data

val destVarType : data -> Name.name

val isVarType : data -> bool

(* Type operator type data *)

val mkOpType : TypeOp.typeOp * Type.ty list -> data

val destOpType : data -> TypeOp.typeOp * Type.ty list

val isOpType : data -> bool

(* Constant data *)

val destConst : data -> Const.const

val isConst : data -> bool

val equalConst : Const.const -> data -> bool

(* Term variable data *)

val destVar : data -> Var.var

val isVar : data -> bool

(* Term data *)

val destTerm : data -> Term.term

val isTerm : data -> bool

(* Term list data *)

val mkTerms : Term.term list -> data

val destTerms : data -> Term.term list

val isTerms : data -> bool

(* Term variable term data *)

val mkVarTerm : Var.var -> data

val destVarTerm : data -> Var.var

val isVarTerm : data -> bool

(* Constant term data *)

val mkConstTerm : Const.const * Type.ty -> data

val destConstTerm : data -> Const.const * Type.ty

val isConstTerm : data -> bool

(* Function application term data *)

val mkAppTerm : Term.term * Term.term -> data

val destAppTerm : data -> Term.term * Term.term

val isAppTerm : data -> bool

(* Lambda abstraction term data *)

val mkAbsTerm : Var.var * Term.term -> data

val destAbsTerm : data -> Var.var * Term.term

val isAbsTerm : data -> bool

(* Sequent data *)

val mkSequent : Sequent.sequent -> data * data

val destSequent : data * data -> Sequent.sequent

val isSequent : data * data -> bool

(* Theorem data *)

val destThm : data -> Thm.thm

val isThm : data -> bool

(* Type substitution data *)

val mkTypeSubst : TypeSubst.substMap -> data

val destTypeSubst : data -> TypeSubst.substMap

val isTypeSubst : data -> bool

(* Term substitution data *)

val mkTermSubst : TermSubst.substMap -> data

val destTermSubst : data -> TermSubst.substMap

val isTermSubst : data -> bool

(* Substitution data *)

val mkSubst : TypeSubst.substMap * TermSubst.substMap -> data

val destSubst : data -> TypeSubst.substMap * TermSubst.substMap

val isSubst : data -> bool

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

val compare : data * data -> order

val equal : data -> data -> bool

(* ------------------------------------------------------------------------- *)
(* Classes of object data.                                                   *)
(* ------------------------------------------------------------------------- *)

val inDictionary : data -> bool

val termBuilder : data -> bool

(* ------------------------------------------------------------------------- *)
(* Extracting theorems from object data.                                     *)
(* ------------------------------------------------------------------------- *)

val thms : data -> Thm.thm list

(* ------------------------------------------------------------------------- *)
(* Extracting symbols from object data.                                      *)
(* ------------------------------------------------------------------------- *)

val symbol : data -> SymbolTable.table

val symbolAdd : SymbolTable.table -> data -> SymbolTable.table

val symbolAddList : SymbolTable.table -> data list -> SymbolTable.table

(* ------------------------------------------------------------------------- *)
(* Breaking down object data into commands.                                  *)
(* ------------------------------------------------------------------------- *)

val command : data -> Command.command * data list

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

val addSharingTypeOps : data -> Term.sharingTypeOps -> Term.sharingTypeOps

val typeOps : data -> TypeOpSet.set

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val addSharingConsts : data -> Term.sharingConsts -> Term.sharingConsts

val consts : data -> ConstSet.set

(* ------------------------------------------------------------------------- *)
(* Searching for subterms.                                                   *)
(* ------------------------------------------------------------------------- *)

val sharingSearch :
    data -> TermSearch.search -> Term.term option * TermSearch.search

val search : TermSearch.search -> data -> Term.term option

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : data Print.pp

end
