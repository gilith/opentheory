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

val objects : stack -> ObjectProv.object list

(* ------------------------------------------------------------------------- *)
(* Peeking.                                                                  *)
(* ------------------------------------------------------------------------- *)

val peek : stack -> ObjectProv.object

(* ------------------------------------------------------------------------- *)
(* Pushing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val push : stack -> ObjectProv.object -> stack

val push2 : stack -> ObjectProv.object -> ObjectProv.object -> stack

val push5 :
    stack -> ObjectProv.object -> ObjectProv.object -> ObjectProv.object ->
    ObjectProv.object -> ObjectProv.object -> stack

(* ------------------------------------------------------------------------- *)
(* Popping.                                                                  *)
(* ------------------------------------------------------------------------- *)

val pop : stack -> stack * ObjectProv.object

val pop2 : stack -> stack * ObjectProv.object * ObjectProv.object

val pop3 :
    stack -> stack * ObjectProv.object * ObjectProv.object * ObjectProv.object

val pop5 :
    stack ->
    stack * ObjectProv.object * ObjectProv.object * ObjectProv.object *
    ObjectProv.object * ObjectProv.object

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : stack Print.pp

end
