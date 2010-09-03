(* ========================================================================= *)
(* PACKAGE DIRECTORY SYSTEM COMMANDS                                         *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature DirectorySystem =
sig

(* ------------------------------------------------------------------------- *)
(* A type of system commands.                                                *)
(* ------------------------------------------------------------------------- *)

type system

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk :
    {cp : string,
     curl : string,
     echo : string,
     sha : string,
     tar : string,
     touch : string} ->
    system

val dest :
    system ->
    {cp : string,
     curl : string,
     echo : string,
     sha : string,
     tar : string,
     touch : string}

val cp : system -> {cp : string}

val curl : system -> {curl : string}

val echo : system -> {echo : string}

val sha : system -> {sha : string}

val tar : system -> {tar : string}

val touch : system -> {touch : string}

end
