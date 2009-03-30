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

val initial : state

(* ------------------------------------------------------------------------- *)
(* Executing commands.                                                       *)
(* ------------------------------------------------------------------------- *)

val execute :
    {savable : bool} -> Interpretation.interpretation ->
    Command.command -> state -> state

val executeStream :
    {savable : bool} -> Interpretation.interpretation ->
    Command.command Stream.stream -> state -> state

end
