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

val mkTypeOp : symbol -> Name.name -> TypeOp.typeOp

val mkConst : symbol -> Name.name -> Const.const

(* ------------------------------------------------------------------------- *)
(* Adding entries.                                                           *)
(* ------------------------------------------------------------------------- *)

val addTypeOps : symbol -> TypeOpSet.set -> symbol

val addConsts : symbol -> ConstSet.set -> symbol

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : symbol Print.pp

val toString : symbol -> string

end
