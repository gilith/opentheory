(* ========================================================================= *)
(* PACKAGE DIRECTORY CONFIG FILE                                             *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature DirectoryConfig =
sig

(* ------------------------------------------------------------------------- *)
(* A type of repo configuration data.                                        *)
(* ------------------------------------------------------------------------- *)

type repo

val nameRepo : repo -> {name : string}

val urlRepo : repo -> {url : string}

val findRepo : repo list -> {name : string} -> repo option

(* ------------------------------------------------------------------------- *)
(* A type of system configuration data.                                      *)
(* ------------------------------------------------------------------------- *)

type system

val cpSystem : system -> {cp : string}

val curlSystem : system -> {curl : string}

val echoSystem : system -> {echo : string}

val shaSystem : system -> {sha : string}

val tarSystem : system -> {tar : string}

val touchSystem : system -> {touch : string}

(* ------------------------------------------------------------------------- *)
(* A type of configuration data.                                             *)
(* ------------------------------------------------------------------------- *)

type config

val repos : config -> repo list

val system : config -> system

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : config Print.pp

(* ------------------------------------------------------------------------- *)
(* I/O.                                                                      *)
(* ------------------------------------------------------------------------- *)

val fromTextFile : {filename : string} -> config

val toTextFile : {config : config, filename : string} -> unit

(* ------------------------------------------------------------------------- *)
(* The default configuration.                                                *)
(* ------------------------------------------------------------------------- *)

val default : config

end
