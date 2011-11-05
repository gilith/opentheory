(* ========================================================================= *)
(* SYMBOL OBJECTS                                                            *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature ObjectSymbol =
sig

(* ------------------------------------------------------------------------- *)
(* A type of symbol objects.                                                 *)
(* ------------------------------------------------------------------------- *)

type symbol

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty : symbol

(* ------------------------------------------------------------------------- *)
(* Looking up symbols.                                                       *)
(* ------------------------------------------------------------------------- *)

val peekTypeOp : symbol -> TypeOp.typeOp -> Object.object option

val peekConst : symbol -> Const.const -> Object.object option

(* ------------------------------------------------------------------------- *)
(* Adding symbols.                                                           *)
(* ------------------------------------------------------------------------- *)

val addTypeOp : symbol -> Object.object -> symbol

val addConst : symbol -> Object.object -> symbol

(* ------------------------------------------------------------------------- *)
(* Harvesting symbols from objects (and their proofs).                       *)
(* ------------------------------------------------------------------------- *)

val addObject : symbol -> Object.object -> symbol

end
