(* ========================================================================= *)
(* OBJECT DICTIONARIES                                                       *)
(* Copyright (c) 2004 Joe Hurd, distributed under the MIT license            *)
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

val define : dict -> key * Object.object -> dict

val refer : dict -> key -> Object.object

val remove : dict -> key -> dict * Object.object

end
