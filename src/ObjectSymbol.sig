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

val peekTypeOp : symbol -> TypeOp.typeOp -> ObjectProv.object option

val peekConst : symbol -> Const.const -> ObjectProv.object option

(* ------------------------------------------------------------------------- *)
(* Adding symbols.                                                           *)
(* ------------------------------------------------------------------------- *)

val addTypeOp : symbol -> ObjectProv.object -> symbol

val addConst : symbol -> ObjectProv.object -> symbol

(* ------------------------------------------------------------------------- *)
(* Harvesting symbols from proofs.                                           *)
(* ------------------------------------------------------------------------- *)

val fromExport : ObjectExport.export -> symbol

end
