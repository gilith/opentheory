(* ========================================================================= *)
(* OBJECT STACKS                                                             *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature ObjectStack =
sig

(* ------------------------------------------------------------------------- *)
(* A type of object stacks.                                                  *)
(* ------------------------------------------------------------------------- *)

type stack

val empty : stack

val size : stack -> int

val null : stack -> bool

val push : stack -> ObjectProv.object -> stack

val peek : stack -> ObjectProv.object

val pop : stack -> stack * ObjectProv.object

val pop2 : stack -> stack * ObjectProv.object * ObjectProv.object

end
