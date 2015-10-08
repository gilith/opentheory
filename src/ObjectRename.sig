(* ========================================================================= *)
(* RENAMING SYMBOLS IN OPENTHEORY OBJECTS                                    *)
(* Copyright (c) 2015 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature ObjectRename =
sig

(* ------------------------------------------------------------------------- *)
(* Object renaming: return NONE for unchanged.                               *)
(* ------------------------------------------------------------------------- *)

type rename

val new : (Symbol.symbol -> Name.name option) -> rename

val id : rename

val sharingRename : Object.object -> rename -> Object.object option * rename

val rename : rename -> Object.object -> Object.object option

end
