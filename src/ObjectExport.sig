(* ========================================================================= *)
(* EXPORTED THEOREM OBJECTS                                                  *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature ObjectExport =
sig

(* ------------------------------------------------------------------------- *)
(* A type of exported theorem objects.                                       *)
(* ------------------------------------------------------------------------- *)

type export

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty : export

val null : export -> bool

val size : export -> int

val insert : export -> ObjectProv.object * Thm.thm -> export

val foldl : (ObjectProv.object * Thm.thm * 's -> 's) -> 's -> export -> 's

val toMap : export -> Thm.thm ObjectProvMap.map

val toList : export -> (ObjectProv.object * Thm.thm) list

(* ------------------------------------------------------------------------- *)
(* Compression.                                                              *)
(* ------------------------------------------------------------------------- *)

val compress : export -> export

end
