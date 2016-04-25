(* ========================================================================= *)
(* CONFIGURATION FILES                                                       *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* A configuration file has the following format:                            *)
(*                                                                           *)
(* # Comment lines can appear anywhere and begin with a # symbol             *)
(*                                                                           *)
(* [Section 1]                                                               *)
(* key = value                                                               *)
(* ...                                                                       *)
(* key = value                                                               *)
(*                                                                           *)
(* [Section 2]                                                               *)
(* key = value                                                               *)
(* etc.                                                                      *)
(* ------------------------------------------------------------------------- *)

signature Config =
sig

(* ------------------------------------------------------------------------- *)
(* A type of configuration files.                                            *)
(* ------------------------------------------------------------------------- *)

datatype keyValue =
    KeyValue of
      {key : string,
       value : string}

datatype section =
    Section of
      {name : string,
       keyValues : keyValue list}

datatype config =
    Config of
      {sections : section list}

val empty : config

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppKey : {key : string} Print.pp

val ppKeyValue : keyValue Print.pp

val ppSectionName : {name : string} Print.pp

val ppSection : section Print.pp

val pp : config Print.pp

val toStringKey : {key : string} -> string

val toStringSectionName : {name : string} -> string

(* ------------------------------------------------------------------------- *)
(* I/O.                                                                      *)
(* ------------------------------------------------------------------------- *)

val toTextFile : {config : config, filename : string} -> unit

val fromTextFile : {filename : string} -> config

end
