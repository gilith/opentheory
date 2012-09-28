(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYMBOL TABLES                                          *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature SymbolTable =
sig

(* ------------------------------------------------------------------------- *)
(* A type of symbol tables.                                                  *)
(* ------------------------------------------------------------------------- *)

type table

val empty : table

val isEmpty : table -> bool

val symbols : table -> SymbolSet.set

val typeOps : table -> TypeOpSet.set

val consts : table -> ConstSet.set

(* ------------------------------------------------------------------------- *)
(* Looking up entries by name.                                               *)
(* ------------------------------------------------------------------------- *)

val peekTypeOp : table -> Name.name -> TypeOp.typeOp option

val peekConst : table -> Name.name -> Const.const option

val knownTypeOp : table -> Name.name -> bool

val knownConst : table -> Name.name -> bool

val mkTypeOp : table -> Name.name -> TypeOp.typeOp

val mkConst : table -> Name.name -> Const.const

(* ------------------------------------------------------------------------- *)
(* Adding entries.                                                           *)
(* ------------------------------------------------------------------------- *)

val addTypeOp : table -> TypeOp.typeOp -> table

val addTypeOpSet : table -> TypeOpSet.set -> table

val addConst : table -> Const.const -> table

val addConstSet : table -> ConstSet.set -> table

val addSymbol : table -> Symbol.symbol -> table

val addSymbolList : table -> Symbol.symbol list -> table

val addSymbolSet : table -> SymbolSet.set -> table

val addType : table -> Type.ty -> table

val addTypeList : table -> Type.ty list -> table

val addTypeSet : table -> TypeSet.set -> table

val addVar : table -> Var.var -> table

val addVarList : table -> Var.var list -> table

val addVarSet : table -> VarSet.set -> table

val addTerm : table -> Term.term -> table

val addTermList : table -> Term.term list -> table

val addTermSet : table -> TermSet.set -> table

val addTermAlphaSet : table -> TermAlphaSet.set -> table

val addSequent : table -> Sequent.sequent -> table

val addSequentList : table -> Sequent.sequent list -> table

val addSequentSet : table -> SequentSet.set -> table

(* ------------------------------------------------------------------------- *)
(* Merging symbol tables.                                                    *)
(* ------------------------------------------------------------------------- *)

val union : table -> table -> table

val unionList : table list -> table

(* ------------------------------------------------------------------------- *)
(* Partition symbol table entries into undefined and defined.                *)
(* ------------------------------------------------------------------------- *)

val partitionUndef : table -> {undefined : table, defined : table}

val undefined : table -> table

val defined : table -> table

val existsUndefined : table -> bool

val existsDefined : table -> bool

val allUndefined : table -> bool

val allDefined : table -> bool

(* ------------------------------------------------------------------------- *)
(* Instantiating undefined type operators and constants with definitions.    *)
(* ------------------------------------------------------------------------- *)

val instType : table -> Type.ty' -> Type.ty option

val instTerm : table -> Term.term' -> Term.term option

val inst : table -> TermRewrite.rewrite

(* ------------------------------------------------------------------------- *)
(* Primitive symbols.                                                        *)
(* ------------------------------------------------------------------------- *)

val primitives : table

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : table Print.pp

val toString : table -> string

end
