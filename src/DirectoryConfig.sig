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

val nameRepo : repo -> DirectoryRepo.name

val urlRepo : repo -> {url : string}

val refreshRepo : repo -> Time.time

val findRepo : repo list -> DirectoryRepo.name -> repo option

(* ------------------------------------------------------------------------- *)
(* A type of license configuration data.                                     *)
(* ------------------------------------------------------------------------- *)

type license

val nameLicense : license -> {name : string}

val urlLicense : license -> {url : string}

val findLicense : license list -> {name : string} -> license option

(* ------------------------------------------------------------------------- *)
(* A type of cleanup configuration data.                                     *)
(* ------------------------------------------------------------------------- *)

type cleanup

val autoCleanup : cleanup -> Time.time option

(* ------------------------------------------------------------------------- *)
(* A type of install configuration data.                                     *)
(* ------------------------------------------------------------------------- *)

type install

val minimalInstall : install -> bool

(* ------------------------------------------------------------------------- *)
(* A type of configuration data.                                             *)
(* ------------------------------------------------------------------------- *)

type config

val repos : config -> repo list

val licenses : config -> license list

val cleanup : config -> cleanup

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
(* Default configurations.                                                   *)
(* ------------------------------------------------------------------------- *)

val default : config

val repoDefault : config

end
