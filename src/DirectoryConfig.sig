(* ========================================================================= *)
(* PACKAGE DIRECTORY CONFIG FILE                                             *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature DirectoryConfig =
sig

(* ------------------------------------------------------------------------- *)
(* A type of install configuration data.                                     *)
(* ------------------------------------------------------------------------- *)

type install

val cleanupInstall : install -> Time.time

(* ------------------------------------------------------------------------- *)
(* A type of repo configuration data.                                        *)
(* ------------------------------------------------------------------------- *)

type repo

val nameRepo : repo -> {name : string}

val urlRepo : repo -> {url : string}

val refreshRepo : repo -> Time.time

val findRepo : repo list -> {name : string} -> repo option

(* ------------------------------------------------------------------------- *)
(* A type of configuration data.                                             *)
(* ------------------------------------------------------------------------- *)

type config

val repos : config -> repo list

val install : config -> install

val system : config -> DirectorySystem.system

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
