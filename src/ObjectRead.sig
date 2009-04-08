(* ========================================================================= *)
(* READING OBJECTS FROM COMMANDS                                             *)
(* Copyright (c) 2004-2009 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature ObjectRead =
sig

(* ------------------------------------------------------------------------- *)
(* A type of states for reading objects from commands.                       *)
(* ------------------------------------------------------------------------- *)

type state

val initial : ObjectThms.thms -> state

val stack : state -> ObjectStack.stack

val dict : state -> ObjectDict.dict

val saved : state -> ObjectThms.thms

(* ------------------------------------------------------------------------- *)
(* Executing commands.                                                       *)
(* ------------------------------------------------------------------------- *)

val execute :
    {savable : bool,
     known : ObjectThms.thms,
     interpretation : Interpretation.interpretation} ->
    Command.command -> state -> state

val executeStream :
    {savable : bool,
     known : ObjectThms.thms,
     interpretation : Interpretation.interpretation} ->
    Command.command Stream.stream -> state -> state

(* ------------------------------------------------------------------------- *)
(* Executing text files.                                                     *)
(* ------------------------------------------------------------------------- *)

val executeTextFile :
    {savable : bool,
     known : ObjectThms.thms,
     interpretation : Interpretation.interpretation,
     filename : string} ->
    state -> state

end
