(* ========================================================================= *)
(* OBJECT STACKS                                                             *)
(* Copyright (c) 2004-2009 Joe Hurd, distributed under the GNU GPL version 2 *)
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

val frameSize : stack -> int

val objects : stack -> ObjectProv.object list

val thms : stack -> ObjectThms.thms

val symbol : stack -> Symbol.symbol

val push : stack -> ObjectProv.object -> stack

val pop : stack -> int -> stack

val peek : stack -> int -> ObjectProv.object

val pop1 : stack -> stack * ObjectProv.object

val pop2 : stack -> stack * ObjectProv.object * ObjectProv.object

val popCall : stack -> stack * Name.name

val topCall : stack -> ObjectProv.object option

val callStack : stack -> ObjectProv.object list

val search : stack -> Sequent.sequent -> (Thm.thm * ObjectProv.object) option

(* ------------------------------------------------------------------------- *)
(* Generating commands to keep the call stack consistent.                    *)
(* ------------------------------------------------------------------------- *)

val alignCalls :
    {call : ObjectProv.object option} -> stack -> stack * Command.command list

(* ------------------------------------------------------------------------- *)
(* Building objects using data on a stack.                                   *)
(* ------------------------------------------------------------------------- *)

val buildObject :
    {savable : bool} -> stack -> Object.object -> ObjectProv.object

(* ------------------------------------------------------------------------- *)
(* The stack is also used to keep track of simulated theorems.               *)
(* ------------------------------------------------------------------------- *)

val symbolSimulation : stack -> Symbol.symbol

val addSimulation : stack -> ThmSet.set * ObjectProv.object -> stack

val searchSimulation :
    stack -> Sequent.sequent -> (Thm.thm * ObjectProv.object) option

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val topCallToString : stack -> string

end
