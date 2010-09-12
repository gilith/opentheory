(* ========================================================================= *)
(* OBJECT DICTIONARIES                                                       *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature ObjectDict =
sig

(* ------------------------------------------------------------------------- *)
(* A type of object dictionaries.                                            *)
(* ------------------------------------------------------------------------- *)

type key = int

type dict

val empty : dict

val size : dict -> int

val define : dict -> key * ObjectProv.object -> dict

val refer : dict -> key -> ObjectProv.object

val remove : dict -> key -> dict * ObjectProv.object

end
