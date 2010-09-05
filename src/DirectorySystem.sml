(* ========================================================================= *)
(* PACKAGE DIRECTORY SYSTEM COMMANDS                                         *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure DirectorySystem :> DirectorySystem =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of system commands.                                                *)
(* ------------------------------------------------------------------------- *)

datatype system =
    System of
      {chmod : string,
       cp : string,
       curl : string,
       echo : string,
       sha : string,
       tar : string};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk data = System data;

fun dest (System data) = data;

fun chmod (System {chmod = x, ...}) = {chmod = x};

fun cp (System {cp = x, ...}) = {cp = x};

fun curl (System {curl = x, ...}) = {curl = x};

fun echo (System {echo = x, ...}) = {echo = x};

fun sha (System {sha = x, ...}) = {sha = x};

fun tar (System {tar = x, ...}) = {tar = x};

end
