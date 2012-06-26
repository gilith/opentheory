(* ========================================================================= *)
(* PACKAGE DIRECTORY SYSTEM COMMANDS                                         *)
(* Copyright (c) 2010 Joe Hurd, distributed under the MIT license            *)
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
    {chmod : string,
     cp : string,
     curl : string,
     echo : string,
     sha : string,
     tar : string} ->
    system

val dest :
    system ->
    {chmod : string,
     cp : string,
     curl : string,
     echo : string,
     sha : string,
     tar : string}

val chmod : system -> {chmod : string}

val cp : system -> {cp : string}

val curl : system -> {curl : string}

val echo : system -> {echo : string}

val sha : system -> {sha : string}

val tar : system -> {tar : string}

end
