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

val mkTypeOp : symbol list -> Name.name -> TypeOp.typeOp

val mkConst : symbol list -> Name.name -> Const.const

(* ------------------------------------------------------------------------- *)
(* Adding entries.                                                           *)
(* ------------------------------------------------------------------------- *)

val addTypeOp : symbol -> TypeOp.typeOp -> symbol

val addTypeOpSet : symbol -> TypeOpSet.set -> symbol

val addConst : symbol -> Const.const -> symbol

val addConstSet : symbol -> ConstSet.set -> symbol

val addSequent : symbol -> Sequent.sequent -> symbol

val addSequentSet : symbol -> SequentSet.set -> symbol

(* ------------------------------------------------------------------------- *)
(* Merging symbol tables.                                                    *)
(* ------------------------------------------------------------------------- *)

val union : symbol -> symbol -> symbol

(* ------------------------------------------------------------------------- *)
(* Partition symbol table entries into undefined and defined.                *)
(* ------------------------------------------------------------------------- *)

val partitionUndef : symbol -> symbol * symbol

(* ------------------------------------------------------------------------- *)
(* Redefining type operators and constants to match symbol table entries.    *)
(* ------------------------------------------------------------------------- *)

(* Preserving sharing *)

type sharingRedef

val newSharingRedef : symbol -> sharingRedef

val sharingRedefType : Type.ty -> sharingRedef -> Type.ty option * sharingRedef

val sharingRedefVar :
    Var.var -> sharingRedef -> Var.var option * sharingRedef

val sharingRedefTerm :
    Term.term -> sharingRedef -> Term.term option * sharingRedef

val sharingRedefSequent :
    Sequent.sequent -> sharingRedef -> Sequent.sequent option * sharingRedef

(* Simple versions *)

val redefTypeOp : symbol -> TypeOp.typeOp -> TypeOp.typeOp option

val redefConst : symbol -> Const.const -> Const.const option

val redefType : symbol -> Type.ty -> Type.ty option

val redefVar : symbol -> Var.var -> Var.var option

val redefTerm : symbol -> Term.term -> Term.term option

val redefSequent : symbol -> Sequent.sequent -> Sequent.sequent option

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : symbol Print.pp

val toString : symbol -> string

end
