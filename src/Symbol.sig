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
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : symbol Print.pp

val toString : symbol -> string

end
