(* ========================================================================= *)
(* READING OBJECTS FROM COMMANDS                                             *)
(* Copyright (c) 2004 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

signature ObjectRead =
sig

(* ------------------------------------------------------------------------- *)
(* A type of parameters for reading objects from commands.                   *)
(* ------------------------------------------------------------------------- *)

type parameters =
     {import : ObjectThms.thms,
      interpretation : Interpretation.interpretation,
      savable : bool}

(* ------------------------------------------------------------------------- *)
(* A type of states for reading objects from commands.                       *)
(* ------------------------------------------------------------------------- *)

type state

val initial : parameters -> state

val parameters : state -> parameters

val stack : state -> ObjectStack.stack

val dict : state -> ObjectDict.dict

val export : state -> ObjectExport.export

val inference : state -> Inference.inference

(* ------------------------------------------------------------------------- *)
(* Executing commands.                                                       *)
(* ------------------------------------------------------------------------- *)

val execute : Command.command -> state -> state

val executeStream : Command.command Stream.stream -> state -> state

(* ------------------------------------------------------------------------- *)
(* Executing text files.                                                     *)
(* ------------------------------------------------------------------------- *)

val executeTextFile : {filename : string} -> state -> state

(* ------------------------------------------------------------------------- *)
(* The exported theorems.                                                    *)
(* ------------------------------------------------------------------------- *)

val thms : state -> ObjectThms.thms

end
