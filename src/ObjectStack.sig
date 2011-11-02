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

val objects : stack -> Object.object list

(* ------------------------------------------------------------------------- *)
(* Peeking.                                                                  *)
(* ------------------------------------------------------------------------- *)

val peek : stack -> Object.object

(* ------------------------------------------------------------------------- *)
(* Pushing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val push : stack -> Object.object -> stack

val push2 : stack -> Object.object -> Object.object -> stack

val push5 :
    stack -> Object.object -> Object.object -> Object.object ->
    Object.object -> Object.object -> stack

(* ------------------------------------------------------------------------- *)
(* Popping.                                                                  *)
(* ------------------------------------------------------------------------- *)

val pop : stack -> stack * Object.object

val pop2 : stack -> stack * Object.object * Object.object

val pop3 :
    stack -> stack * Object.object * Object.object * Object.object

val pop5 :
    stack ->
    stack * Object.object * Object.object * Object.object *
    Object.object * Object.object

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : stack Print.pp

end
