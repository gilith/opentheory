(* ========================================================================= *)
(* REPOSITORY CONFIG FILE                                                    *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature RepositoryConfig =
sig

(* ------------------------------------------------------------------------- *)
(* A type of remote repository configuration data.                           *)
(* ------------------------------------------------------------------------- *)

type remote

val nameRemote : remote -> RepositoryRemote.name

val urlRemote : remote -> {url : string}

val refreshRemote : remote -> Time.time

val findRemote : remote list -> RepositoryRemote.name -> remote option

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

val authors : config -> PackageAuthor.author list

val remotes : config -> remote list

val licenses : config -> license list

val cleanup : config -> cleanup

val install : config -> install

val system : config -> RepositorySystem.system

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

val remoteDefault : config

end
