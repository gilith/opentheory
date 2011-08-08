(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYMBOL TABLES                                          *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Symbol =
sig

(* ------------------------------------------------------------------------- *)
(* A type of symbol tables.                                                  *)
(* ------------------------------------------------------------------------- *)

type symbol

val empty : symbol

val typeOps : symbol -> TypeOpSet.set

val consts : symbol -> ConstSet.set

(* ------------------------------------------------------------------------- *)
(* Looking up entries.                                                       *)
(* ------------------------------------------------------------------------- *)

val peekTypeOp : symbol -> Name.name -> TypeOp.typeOp option

val peekConst : symbol -> Name.name -> Const.const option

val knownTypeOp : symbol -> Name.name -> bool

val knownConst : symbol -> Name.name -> bool

val mkTypeOp : symbol -> Name.name -> TypeOp.typeOp

val mkConst : symbol -> Name.name -> Const.const

(* ------------------------------------------------------------------------- *)
(* Adding entries.                                                           *)
(* ------------------------------------------------------------------------- *)

val addTypeOp : symbol -> TypeOp.typeOp -> symbol

val addTypeOpSet : symbol -> TypeOpSet.set -> symbol

val addConst : symbol -> Const.const -> symbol

val addConstSet : symbol -> ConstSet.set -> symbol

val addType : symbol -> Type.ty -> symbol

val addTypeList : symbol -> Type.ty list -> symbol

val addTypeSet : symbol -> TypeSet.set -> symbol

val addVar : symbol -> Var.var -> symbol

val addVarList : symbol -> Var.var list -> symbol

val addVarSet : symbol -> VarSet.set -> symbol

val addTerm : symbol -> Term.term -> symbol

val addTermList : symbol -> Term.term list -> symbol

val addTermSet : symbol -> TermSet.set -> symbol

val addTermAlphaSet : symbol -> TermAlphaSet.set -> symbol

val addSequent : symbol -> Sequent.sequent -> symbol

val addSequentList : symbol -> Sequent.sequent list -> symbol

val addSequentSet : symbol -> SequentSet.set -> symbol

(* ------------------------------------------------------------------------- *)
(* Merging symbol tables.                                                    *)
(* ------------------------------------------------------------------------- *)

val union : symbol -> symbol -> symbol

val unionList : symbol list -> symbol

(* ------------------------------------------------------------------------- *)
(* Partition symbol table entries into undefined and defined.                *)
(* ------------------------------------------------------------------------- *)

val partitionUndef : symbol -> {undefined : symbol, defined : symbol}

(* ------------------------------------------------------------------------- *)
(* Instantiating undefined type operators and constants with definitions.    *)
(* ------------------------------------------------------------------------- *)

val instType : symbol -> Type.ty' -> Type.ty option

val instTerm : symbol -> Term.term' -> Term.term option

val inst : symbol -> TermRewrite.rewrite

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : symbol Print.pp

val toString : symbol -> string

end
